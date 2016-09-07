;;; multi-line-ruby-test.el --- multi-line ruby tests -*- lexical-binding: t; -*-

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

;; Test the behavior of multi-line in ruby-mode

;;; Code:

(require 'cl-lib)
(require 'shut-up)

(require 'multi-line)
(require 'multi-line-test)

(put 'multi-line-deftest-ruby 'lisp-indent-function '(lambda (&rest args) 0))

(defun multi-line-test-ruby-setup ()
  (shut-up (ruby-mode))
  (setq fill-column 80
        indent-tabs-mode nil))

(cl-defmacro multi-line-deftest-ruby
    (name initial expected &rest args &key tags setup &allow-other-keys)
  (let ((tags (quote (cons 'python tags)))
        (setup (cons '(multi-line-test-ruby-setup) setup)))
    `(multi-line-deftest ,name ,initial ,expected :tags (quote ,tags) :setup ,setup
                         ,@args)))

(multi-line-deftest-ruby test-ruby-hash-literal
"{:avalue => \"cool\", :greater => \"fun\", :avalue3 => \"cool\", :greaterg => \"fun\", :more => \"coool real long\",}"
"{
  :avalue => \"cool\", :greater => \"fun\", :avalue3 => \"cool\", :greaterg => \"fun\",
  :more => \"coool real long\",
}"
:setup ((search-forward "a")))

(provide 'multi-line-ruby-test)
;;; multi-line-ruby-test.el ends here
