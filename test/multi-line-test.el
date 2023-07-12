;;; multi-line-test.el --- multi-line test suite -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2023 Ivan Malison

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The unit test suite of multi-line

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'ert)
(require 'shut-up)

(require 'multi-line)

(put 'multi-line-deftest 'lisp-indent-function '(lambda (&rest args) 0))

(defun multi-line-whitespace-trim (original)
  (split-string original nil nil "[[:space:]]"))

(cl-defmacro multi-line-deftest
    (name initial-text expected-text &key strategy tags setup)
  (let ((expected-texts (if (listp expected-text)
                            expected-text
                          `(list ,expected-text)))
        (test-name (intern (concat "multi-line-" (symbol-name name)))))
    `(ert-deftest ,test-name ()
       :tags ,tags
       (with-temp-buffer
         (insert ,initial-text)
         ,(when strategy
            `(setq multi-line-current-strategy ,strategy))
         (goto-char (point-min))
         ,@setup
         (cl-loop for expected-text in ,expected-texts
                  do (multi-line nil)
                  (should
                   (equal (multi-line-whitespace-trim expected-text)
                          (multi-line-whitespace-trim (buffer-string))))))
       t)))

(put 'multi-line-deftest-for-mode 'lisp-indent-function '(lambda (&rest args) 0))

(cl-defmacro multi-line-deftest-for-mode
    (mode name initial expected &rest args &key tags setup &allow-other-keys)
  (let ((new-tags (cons mode tags))
        (setup (cons `(progn
                        (shut-up (,(intern (concat (symbol-name mode) "-mode"))))
                        (setq fill-column 80
                              indent-tabs-mode nil))
                     setup)))
    `(multi-line-deftest ,name ,initial ,expected :tags (quote ,new-tags) :setup ,setup ,@args)))

(provide 'multi-line-test)
;;; multi-line-test.el ends here
