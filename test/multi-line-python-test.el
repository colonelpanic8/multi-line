;;; multi-line-python-test.el --- multi-line python tests -*- lexical-binding: t; -*-

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

;; Test the behavior of multi-line in python-mode

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'shut-up)

(require 'multi-line)
(require 'multi-line-test)

(put 'multi-line-deftest-python 'lisp-indent-function '(lambda (&rest args) 0))

(defun multi-line-test-python-setup ()
  (shut-up (python-mode))
  (setq fill-column 80
        indent-tabs-mode nil))

(cl-defmacro multi-line-deftest-python
    (name initial expected &rest args &key tags setup &allow-other-keys)
  (let ((tags (quote (cons 'python tags)))
        (setup (cons '(multi-line-test-python-setup) setup)))
    `(multi-line-deftest ,name ,initial ,expected :tags (quote ,tags) :setup ,setup
                         ,@args)))

(multi-line-deftest-python test-basic-python
"
function(nested(fdasfdsaf, fdasfdsaf, fdasfdsaf, fdasfdsa), other, next, another_nested_call(more, cool, quite))
"
(list
 "
function(
    nested(fdasfdsaf, fdasfdsaf, fdasfdsaf, fdasfdsa), other, next,
    another_nested_call(more, cool, quite),
)
"
 "
function(
    nested(fdasfdsaf, fdasfdsaf, fdasfdsaf, fdasfdsa),
    other,
    next,
    another_nested_call(more, cool, quite),
)
"
 "
function(nested(fdasfdsaf, fdasfdsaf, fdasfdsaf, fdasfdsa), other, next,
         another_nested_call(more, cool, quite))
"
 "
function(nested(fdasfdsaf, fdasfdsaf, fdasfdsaf, fdasfdsa), other, next, another_nested_call(more, cool, quite))
")
:setup ((search-forward "(") (forward-char)))

(multi-line-deftest-python test-python-one-argument
"
fdsafdsafdsafdsafdsafdsafdsafdsafdsa(fdsafdsafdsafdsafdsafdsafdsafdfdsasdfdsadfdsaddffdsaf)
"
"
fdsafdsafdsafdsafdsafdsafdsafdsafdsa(
    fdsafdsafdsafdsafdsafdsafdsafdfdsasdfdsadfdsaddffdsaf,
)
"
:setup ((search-forward "(") (forward-char)))

(multi-line-deftest-python test-python-nested-dict
"
function(nested(fdasfdsaf, fdasfdsaf, fdasfdsaf, fdasfdsa), {
    'a': 'bfdsafdsafdsafdsafdsafdsafdsafdsafdsafdsa',
    'c': 'bfdsafdjksalf;djsakf;djsaklf;fdjksal;fdsa',
}, next, another_nested_call(more, cool, quite))
"
(list "
function(
    nested(fdasfdsaf, fdasfdsaf, fdasfdsaf, fdasfdsa), {
        'a': 'bfdsafdsafdsafdsafdsafdsafdsafdsafdsafdsa',
        'c': 'bfdsafdjksalf;djsakf;djsaklf;fdjksal;fdsa',
    }, next, another_nested_call(more, cool, quite),
)
"
"
function(
    nested(fdasfdsaf, fdasfdsaf, fdasfdsaf, fdasfdsa),
    {
        'a': 'bfdsafdsafdsafdsafdsafdsafdsafdsafdsafdsa',
        'c': 'bfdsafdjksalf;djsakf;djsaklf;fdjksal;fdsa',
    },
    next,
    another_nested_call(more, cool, quite),
)
"
"
function(nested(fdasfdsaf, fdasfdsaf, fdasfdsaf, fdasfdsa), {
    'a': 'bfdsafdsafdsafdsafdsafdsafdsafdsafdsafdsa',
    'c': 'bfdsafdjksalf;djsakf;djsaklf;fdjksal;fdsa',
}, next, another_nested_call(more, cool, quite))
")
:setup ((search-forward "(") (forward-char)))

(provide 'multi-line-python-test)
;;; multi-line-python-test.el ends here
