;;; ob-coq-tests.el --- ERT tests for ob-coq  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Michael Herstine <sp1ff@pobox.com>

;; Author: Michael Herstine <sp1ff@pobox.com>

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

;; Unit tests for ob-coq.

;;; Code:

(require 'ert)
(require 'org)

(require 'ob-coq)

(ert-deftest ob-coq-tests-inf-coq ()
  "Test the `inf-coq' package."

  (should (equal "inf-coq" (inf-coq--buffer-for-session nil t)))
  (should (equal "*inf-coq*" (inf-coq--buffer-for-session)))
  (should (equal "*inf-coq-foo*" (inf-coq--buffer-for-session "foo")))
  (should (equal "inf-coq-foo" (inf-coq--buffer-for-session "foo" t)))

  (defconst session "ob-coq-tests-inf-coq")
  (inf-coq-run-coq session)
  (sit-for 0.1)
  (let ((results
         (inf-coq-send-string "Inductive bool : Set :=
| false
| true.
Definition negb (x : bool) : bool :=
match x with
| false => true
| true => false
end.
Compute negb true.
" session)))
    (message "results: %s" results)
    (should (equal results "bool is defined
bool_rect is defined
bool_ind is defined
bool_rec is defined
bool_sind is defined

negb is defined

     = false
     : bool

")))
  ;; iss-01 "`inf-coq-quit' sometimes prompts interactively
  (let ((inf-coq-process-shutdown-wait-time 0.0))
    ;; This won't quite *fail* if this bug reappears, it'll just hang the test
    ;; suite
    (inf-coq-quit session)))

(ert-deftest ob-coq-tests-babel-smoke ()
  "Smoke tests for ob-coq."

  ;; Write `test-doc.org` to a temporary buffer (because simply opening it via
  ;; `find-file' on a read-only filesystem will leave the buffer in read-only
  ;; mode and source code evaluation will fail.
  (let* ((buffer (find-file (concat (getenv "srcdir") "/test-doc.org")))
         (text (with-current-buffer buffer (buffer-string)))
         (org-confirm-babel-evaluate nil))
    (with-temp-buffer
      (insert text)
      (org-mode)
      (goto-char 199)
      (org-ctrl-c-ctrl-c)
      (goto-char 229)
      (should (looking-at "#\\+RESULTS:
: 
: bool is defined
: bool_rect is defined
: bool_ind is defined
: bool_rec is defined
: bool_sind is defined
:")))))

(provide 'ob-coq-tests)

;;; ob-coq-tests.el ends here
