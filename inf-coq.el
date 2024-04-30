;;; inf-coq.el --- Interaction with inferior Coq processes   -*- lexical-binding: t -*-

;; Copyright (C) 2024 Michael Herstine <sp1ff@pobox.com>

;; Author: Michael Herstine <sp1ff@pobox.com>
;; Created: 31 March 2024
;; Keywords: languages processes tools
;; Package: inf-coq
;; Package-Requires: ((emacs "25.1") (org "9.6"))
;; Homepage: https://github.com/sp1ff/ob-coq
;; Version: 0.0.4

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Coq is a proof assistant (see <http://coq.inria.fr>).  The interactive
;; program with which one interacts is `coqtop`.  This pacakge provides a
;; command interpreter for `coqtop`.  That is to say, it runs `coqtop` as an
;; inferior process and provides a major mode based on `comint' for interacting
;; with it in the process' buffer.  This is a minimal implementation
;; purpose-built for use with `ob-coq'.  For a more fullly featured Coq
;; interface under Emacs look at Proof General
;; (<http://zermelo.dcs.ed.ac.uk/~proofgen>).

;; This implementation is loosely based on the following:

;; - inferior-coq.el :: once provided as part of Coq itself, but removed in
;;   2018 (in commit 41d597866d4f79fe5109c25c6f5cc57d0ebf7f0f) with the comment
;;   "People should use Proof-General"
;; - inf-haskell.el :: part of haskell-mode; not Coq-related, but solves
;;   a similar problem
;; - proof-shell.el :: part of Proof General
;; - scomint.el :: part of Proof General, a "simplified comint" on which the
;;   Proof General `coqtop` interaction is built
;; - comint.el :: Emacs' "command interpreter" base
;;   (<https://www.masteringemacs.org/article/comint-writing-command-interpreter>
;;   has a nice writeup)

;; The primary difference between this implementation and those of
;; inferior-coq.el and inf-haskell.el is that the latter two support one & only
;; one process communicating with their respective interpreters.  Given that
;; this implementation is designed to support an Org Babel backend for Coq, I
;; wanted to be able to support multiple sessions (that, and the fact that
;; Proof General can only support one sessoin at a time has annoyed me for some
;; time).

;;; Code:
(require 'cl-lib)
(require 'comint)

(defgroup inf-coq nil
  "Running coqtop as an inferior process."
  :group 'comm)

(defcustom inf-coq-program-name "coqtop"
  "Path to the coqtop program."
  :group 'inf-coq
  :type 'string)

(defcustom inf-coq-program-args '()
  "List of arguments to pass to `inf-coq-program-name'."
  :group 'inf-coq
  :type '(repeat string))

(defcustom inf-coq-process-shutdown-wait-time 0.1
  "Time, in seconds, to wait for coqtop process shutdown when closing buffers."
  :group 'inf-coq
  :type 'float)

(defcustom inf-coq-mode-hook nil
  "Hook invoked on entry to `inf-coq-mode'."
  :group 'inf-coq
  :type 'hook)

(defvar inf-coq-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    map)
  "Keymap for `inf-coq' mode.")

;; According to
;; <https://coq.inria.fr/doc/V8.19.1/refman/language/core/basic.html?highlight=identifiers>
;; Coq identifiers obey:

;; ident ::= first-letter subsequent-letter*
;; first-letter ::= [a-zA-Z_<unicode letter>
;; subsequent-letter ::= first-letter digit ' <unicode id part>

;; I'm not sure what is meant by a "unicode letter" or a "unicode id part"; the
;; docs go on to say: unicode-letter "non-exhaustively includes Latin, Greek,
;; Gothic, Cyrillic, Arabic, Hebrew, Georgian, Hangul, Hiragana and Katakana
;; characters, CJK ideographs, mathematical letter-like symbols and
;; non-breaking space. [unicoded-id-part] non-exhaustively includes symbols for
;; prime letters and subscripts."

;; I'm not sure how, precisely to map that to a regexp.  Note also that the Coq
;; prompt changes. So far I've seen it change in response to stating a
;; Theorem-- the prompt changes to the name which I'm defining.
(defconst inf-coq-prompt-regexp "^[^0-9[:space:]][^[:space:]]* < "
  "Regular expression matching the Coq prompt.")

;; Coq output will, in general, arrive asynchronously in chunks. This variable
;; tracks each session's output. The value associated with each session
;; identifier is a cons cell, the second element of which is the concatenated
;; text received so far (with prompts removed). The first is the number of
;; "bare" prompts seen so far-- when this reaches zero, we know we've received
;; all the output from the most recent text block.
(defvar inf-coq--output-alist nil
  "Association list mapping sessions to most recent output.")

(defun inf-coq--map-session-internal (session)
  "Map SESSION for internal use.

We use the key \\='default-session for the nil session."
  (or session 'default-session))

(defun inf-coq--preoutput-filter-function (text &optional session)
  "Hook invoked before inserting TEXT into the output buffer.
Optional SESSION names the particular session.

`inf-coq' (ab)uses this to intercept output from commands sent via
`inf-coq-send-string' and redirect it to `inf-coq--output-alist'.

Nb that this cannot be installed as a preoutput hook directly; it will
need to be wrapped in a lambda to pass along the session."

  (let* ((prompts-seen 0)
         (key (inf-coq--map-session-internal session))
         (lines (cl-mapcar
		             (lambda (line)
			             ;; `line' could begin with multiple copies matching
			             ;; `inf-coq-prompt-regexp'...
                   (while (string-match inf-coq-prompt-regexp line)
			               (setq prompts-seen (1+ prompts-seen)
                           line (substring line (match-end 0))))
			             line)
		             (split-string text "\n")))
         (current-state (cdr (assoc key inf-coq--output-alist)))
         (num-dots (cadr (assoc key inf-coq--output-alist)))
         (new-text (mapconcat #'identity lines "\n"))
         (new-num-dots (- num-dots prompts-seen)))
    (setq current-state
          (cons
           new-num-dots
           (if current-state (concat (cdr current-state) new-text) new-text)))
    (setf (alist-get key inf-coq--output-alist nil nil 'equal) current-state)
    ;; Return the empty string, so that nothing will be inserted into the buffer
    ""))

(define-derived-mode inf-coq-mode comint-mode "Inf-Coq"
  "Major mode for interacting with an inferior Coq process.

The following commands are available:
\\{inf-coq-mode-map}"
  :group 'inf-coq
  (setq-local mode-line-process '(":%s")
              ;; make the Coq prompt read-only for purposes of line editing
              comint-prompt-read-only t
              comint-use-prompt-regexp nil
              comint-prompt-regexp inf-coq-prompt-regexp
              paragraph-start inf-coq-prompt-regexp))

(defvar inf-coq--buffer-fmt "inf-coq-%s"
  "Format string for inf-coq buffers.")

(defun inf-coq--buffer-for-session (&optional session no-asterisks)
  "Compute a buffer name for SESSION.
If the optional parameter NO-ASTERISKS is non-nil, don't surround the
buffer base name with asterisks (i.e. make it suitable for use with
`make-comint')."
  (concat
   (unless no-asterisks "*")
   (if session
       (format inf-coq--buffer-fmt session)
     "inf-coq")
   (unless no-asterisks "*")))

(defun inf-coq-run-coq (&optional session)
  "Run an inferior Coq process in session SESSION."
  (interactive
   (list (read-string "Session: ")))
  (let ((buffer-name (inf-coq--buffer-for-session session t)))
    (if (not (comint-check-proc (concat "*" buffer-name "*")))
        (let ((buffer
               (set-buffer
                (apply
                 #'make-comint buffer-name inf-coq-program-name nil inf-coq-program-args))))
          (with-current-buffer buffer
            (inf-coq-mode))))
    (if (called-interactively-p 'any)
      (switch-to-buffer (concat "*" buffer-name "*")))))

(defun inf-coq-process (&optional session)
  "Return the process object associated with SESSION.
Return nil if there is none."
  (get-buffer-process (inf-coq--buffer-for-session session)))

(defun inf-coq-send-string (text &optional session)
  "Send TEXT to the Coq process associated with SESSION & collect the response.

This method will call the inferior Coq process directly, \"bypassing\"
the comint buffer: no input will be inserted into the buffer, nor will
the results."

  (let ((num-dots (cl-count ?. text)))
    (unless (> num-dots 0)
      (error "The input is not a sentence in the vernacular; it will produce no output"))
    (let ((text (if (string-suffix-p "\n" text) text (concat text "\n"))))
      (with-local-quit
        (let* ((comint-preoutput-filter-functions-orig comint-preoutput-filter-functions)
               (proc (inf-coq-process session))
               (key (inf-coq--map-session-internal session))
               (result))
          (setf (alist-get key inf-coq--output-alist nil nil 'equal) (cons num-dots ""))
          (with-current-buffer (process-buffer proc)
            (add-hook
             'comint-preoutput-filter-functions
             (lambda (text)
               (inf-coq--preoutput-filter-function text session)))
            (comint-send-string proc text)
            (let* ((value (cdr (assoc key inf-coq--output-alist))))
              (while (> (car value) 0)
                (accept-process-output proc 0.1)
                (sit-for 0.1)
                (setq value (cdr (assoc key inf-coq--output-alist))))
              (setq result (cdr value)))
            (setf (alist-get key inf-coq--output-alist nil nil 'equal) nil)
            (setf (alist-get key inf-coq--output-alist nil t 'equal) nil)
            (setq comint-preoutput-filter-functions comint-preoutput-filter-functions-orig)
            result))))))

(defun inf-coq-quit (&optional session)
  "Quit the Coq process corresponding to SESSION."
  (with-current-buffer (inf-coq--buffer-for-session session)
    (let ((confirm-kill-processes nil))
      (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)
      (comint-kill-subjob)
      ;; We shouldn't be queried (due to the local binding above and the
      ;; clearing of the "query-on-exit" flag for the process) even if we
      ;; immediately kill the buffer, but still; give the process a chance to
      ;; clean itself up.
      (sit-for inf-coq-process-shutdown-wait-time)
      (kill-buffer))))

(provide 'inf-coq)

;;; inf-coq.el ends here.
