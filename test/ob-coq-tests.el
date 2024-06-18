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
Print negb.
Compute negb true.
" session)))
    (should (equal results "bool is defined
bool_rect is defined
bool_ind is defined
bool_rec is defined
bool_sind is defined

negb is defined

negb =
fun x : bool => match x with
                | false => true
                | true => false
                end
     : bool -> bool

Arguments negb x

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
  (let* ((buffer (find-file (concat (getenv "srcdir") "/corpus/test-doc.org")))
         (text (with-current-buffer buffer (buffer-string)))
         (org-confirm-babel-evaluate nil))
    (with-temp-buffer
      (insert text)
      (org-mode)
      (goto-char 199)
      (org-ctrl-c-ctrl-c)
      (goto-char 229)
      (should
       (looking-at "#\\+RESULTS:
: 
: bool is defined
: bool_rect is defined
: bool_ind is defined
: bool_rec is defined
: bool_sind is defined
:")))))

(ert-deftest ob-coq-tests-issue-2 ()
  "Regression test for Github issue #2 (Truncated output)"

  ;; Write `test-doc.org` to a temporary buffer (because simply opening it via
  ;; `find-file' on a read-only filesystem will leave the buffer in read-only
  ;; mode and source code evaluation will fail.
  (let* ((buffer (find-file (concat (getenv "srcdir") "/corpus/test-doc.org")))
         (text (with-current-buffer buffer (buffer-string)))
         (org-confirm-babel-evaluate nil))
    (with-temp-buffer
      (insert text)
      (org-mode)
      (goto-char 263)
      (org-ctrl-c-ctrl-c)
      (goto-char 318)
      (should
       (looking-at "#\\+RESULTS:
#\\+begin_example

\\[Loading ML file ring_plugin\\.cmxs (using legacy method) \\.\\.\\. done\\]
\\[Loading ML file zify_plugin\\.cmxs (using legacy method) \\.\\.\\. done\\]
\\[Loading ML file micromega_plugin\\.cmxs (using legacy method) \\.\\.\\. done\\]
\\[Loading ML file btauto_plugin\\.cmxs (using legacy method) \\.\\.\\. done\\]

Z_lt_ge_dec =
fun x y : Z => Z_lt_dec x y
     : forall x y : Z, {(x < y)%Z} \\+ {(x >= y)%Z}

Arguments Z_lt_ge_dec (x y)%Z_scope

#\\+end_example
")))))

(ert-deftest ob-coq-tests-blog-post ()
  "Investigate an issue I found while writing a blog post."

  ;; Write `option.org` to a temporary buffer (because simply opening it via
  ;; `find-file' on a read-only filesystem will leave the buffer in read-only
  ;; mode and source code evaluation will fail.
  (let* ((buffer (find-file (concat (getenv "srcdir") "/corpus/option.org")))
         (text (with-current-buffer buffer (buffer-string)))
         (org-confirm-babel-evaluate nil))
    (with-temp-buffer
      (insert text)
      (org-mode)
      (goto-char 213)
      (org-ctrl-c-ctrl-c)
      (goto-char 290)
      (should
       (looking-at "#\\+RESULTS:
: 
: 
: Option is defined
: Option_rect is defined
: Option_ind is defined
: Option_rec is defined
: Option_sind is defined"))
      (goto-char 722)
      (org-ctrl-c-ctrl-c)
      (goto-char 752)
      (should
       (looking-at "#\\+RESULTS:
#\\+begin_example
1 goal
 *
  ============================
  forall x : Option, x <> Fail -> bool
 *
1 goal
 *
  x : Option
  ============================
  Fail <> Fail -> bool
 *
#\\+end_example
"))
      (goto-char 1028)
      (org-ctrl-c-ctrl-c)
      (goto-char 1096)
      (message "Looking at: %s" (buffer-substring (point) (+ (point) 32)))
      (should
       (looking-at "#\\+RESULTS:
#\\+begin_example
1 goal
 *
  x : Option
  H : Fail <> Fail
  ============================
  bool
 *
2 goals
 *
  x : Option
  H : Fail <> Fail
  ============================
  Fail <> Fail
 *
goal 2 is:
 Fail = Fail
 *
1 goal
 *
  x : Option
  H : Fail <> Fail
  ============================
  Fail = Fail
 *
No more goals.
 *
 *
#\\+end_example
")))))

(provide 'ob-coq-tests)

;;; ob-coq-tests.el ends here
