;;; ob-coq.el --- org-babel functions for coq evaluation   -*- lexical-binding: t -*-

;; Copyright (C) 2024 Michael Herstine <sp1ff@pobox.com>

;; Author: Michael Herstine
;; Created: 31 March 2024
;; Keywords: languages processes tools
;; Package: ob-coq
;; Homepage: https://orgmode.org
;; Package: ob-coq
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Org-Babel support for evaluating Coq code blocks.

;;; Requirements:

;; Coq is a proof assistant (see <http://coq.inria.fr>) and coq-mode.

;;; Code:

(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)

(require 'inf-coq)

(defcustom org-babel-coq-command "coqtop"
  "Name of the command for executing Coq code."
  :group 'org-babel
  :type 'string)

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("coq" . "v"))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:coq '())

(defun org-babel-variable-assignments:coq (params)
  "Return a list of Coq statements assigning the block's definitions."
  (mapcar
   (lambda (pair)
     (format "Definition %s := %s."
	     (car pair)
	     (org-babel-coq-var-to-coq (cdr pair))))
   (org-babel--get-vars params)))

(defun org-babel-coq-var-to-coq (var)
  "Convert an elisp var into a string of coq source code
specifying a var of the same value."
  (format "%S" var))

(defun org-babel-execute:coq (body params)
  "Execute a block of Coq code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (message "Executing Coq source code block")
  (let* ((processed-params (org-babel-process-params params))
         ;; set the session if the value of the session keyword is not the
         ;; string `none'
         (session (cdr (assq :session processed-params)))
         (session (if (equal "none" session) nil session))
         (result-params (assq :result-params processed-params))
         (result-type (assq :result-type processed-params))
         (full-body
          (org-babel-expand-body:generic
            body params
            (org-babel-variable-assignments:coq params)))
         (result (org-babel-coq-evaluate session full-body result-type
                                         result-params)))
    (org-babel-reassemble-table
     result
     (org-babel-pick-name (cdr (assq :colname-names params))
			                    (cdr (assq :colnames params)))
     (org-babel-pick-name (cdr (assq :rowname-names params))
			                    (cdr (assq :rownames params))))))

(defun org-babel--strip-welcome (text)
  "Strip the Coq welcome message from TEXT, if present."
  (if (string-match "^Welcome to Coq [0-9]+\\.[0-9]+\\.[0-9]+\n" text)
      (substring text (match-end 0))
    text))

(defun org-babel-coq-evaluate
    (session body &optional result-type result-params _preamble)
  "Evaluate BODY as Coq code."

  (let ((proc (inf-coq-process session)))
    (unless proc (inf-coq-run-coq session))
    (unless session
      (with-current-buffer (process-buffer (inf-coq-process session))
        ;; The buffer (and process) will be killed on completion, so no need to
        ;; remove later
        (add-hook 'comint-preoutput-filter-functions #'org-babel--strip-welcome -99)))
    (let ((result (inf-coq-send-string body session)))
      (unless session (inf-coq-quit))
      (let ((result
             (pcase (cdr result-type)
               (`output result)
               (`value (error "Parsing Coq output as values not supported, yet.")))))
        (org-babel-result-cond (cdr result-params)
	        result (when result (org-babel-script-escape result)))))))

(provide 'ob-coq)
;;; ob-coq.el ends here
