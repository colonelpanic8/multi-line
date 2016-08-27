;;; multi-line-test.el --- multi-line test suite -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2016 Ivan Malison

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
(require 'ert)

(require 'multi-line)

(put 'multi-line-deftest 'lisp-indent-function '(lambda (&rest args) 0))

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
                   (equal expected-text (buffer-string)))))
       t)))

(provide 'multi-line-test)
;;; multi-line-test.el ends here
