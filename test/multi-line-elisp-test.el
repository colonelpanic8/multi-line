;;; multi-line-elisp-test.el --- multi-line elisp tests -*- lexical-binding: t; -*-

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

;; Test the behavior of multi-line in emacs-lisp-mode

;;; Code:

(require 'cl-lib)

(require 'multi-line)
(require 'multi-line-test)

(defun multi-line-test-elisp-setup ()
  (emacs-lisp-mode)
  (setq fill-column 80
        indent-tabs-mode nil)
  (forward-char))

(cl-defmacro multi-line-deftest-elisp (name initial expected &rest args
                                            &key tags setup &allow-other-keys)
  (let ((tags  (cons 'elisp tags))
        (setup (cons '(multi-line-test-elisp-setup) setup)))
    `(multi-line-deftest ,name ,initial ,expected :tags (quote ,tags)
                         :setup ,setup ,@args)))

(multi-line-deftest-elisp test-basic-elisp
"(a bbbbbbbbbbbbbbbbbb ccccccccccccccccccccc ddddddddeeeeeeeeekkkkkkkkkkffffffffff gggggggggggg)"
"(a bbbbbbbbbbbbbbbbbb ccccccccccccccccccccc ddddddddeeeeeeeeekkkkkkkkkkffffffffff
   gggggggggggg)")

(multi-line-deftest-elisp test-handles-quoted-lists
"(list '((emacs-lisp-mode) (more-fun-stuff) (setq fill-column 80) (setq indent-tabs-mode nil) (forward-char)))"
"(list '((emacs-lisp-mode) (more-fun-stuff) (setq fill-column 80)
        (setq indent-tabs-mode nil) (forward-char)))"
:setup ((search-forward "(more-fun")
        (up-list)))

(multi-line-deftest-elisp test-handles-quasi-quoted-lists
"(list `((emacs-lisp-mode) (more-fun-stuff) (setq fill-column 80) (setq indent-tabs-mode nil) (forward-char)))"
"(list `((emacs-lisp-mode) (more-fun-stuff) (setq fill-column 80)
        (setq indent-tabs-mode nil) (forward-char)))"
:setup ((search-forward "(more-fun")
        (up-list)))

(multi-line-deftest-elisp test-handles-unquote
"`(,(list 'afdsafdsafdsfafdsafdsafd 'afdsafdsafdasfdsafs 'afdasfdsafdsafdafdsafdsafdsafdsa))"
"`(,(list 'afdsafdsafdsfafdsafdsafd 'afdsafdsafdasfdsafs
         'afdasfdsafdsafdafdsafdsafdsafdsa))"
:setup ((search-forward "fdsafdsafdsfafdsafdsafd")))

(multi-line-deftest-elisp test-handles-splice
"`(,@(list 'afdsafdsafdsfafdsafdsafd 'afdsafdsafdasfdsafs 'afdasfdsafdsafdafdsafdsafdsafdsa))"
"`(,@(list 'afdsafdsafdsfafdsafdsafd 'afdsafdsafdasfdsafs
          'afdasfdsafdsafdafdsafdsafdsafdsa))"
:setup ((search-forward "fdsafdsafdsfafdsafdsafd")))

(multi-line-deftest-elisp colon-keywords-always-paired
"(fdsafdsajklfdjsaklf fdasfdsafdsa fdasfdsafdsafdsafdsaf :f fdsafdsafdfdsafsafdsadf :c fdasfdsafdsafdsafdsafdsafdsafdsafdsafdsa)"
"(fdsafdsajklfdjsaklf fdasfdsafdsa fdasfdsafdsafdsafdsaf
                     :f fdsafdsafdfdsafsafdsadf
                     :c fdasfdsafdsafdsafdsafdsafdsafdsafdsafdsa)")

(multi-line-deftest-elisp test-checks-all-newlines-between-candidates
"(fdsafkldsfdsafdsafdsaf fdsafdsafdsaf fdsafdsafdsa (afdasffda
                                                    fdsafdsafdsaf fdsafdsaf fdsafdsafdsafdsa fdsafdsa) a)"
"(fdsafkldsfdsafdsafdsaf fdsafdsafdsaf fdsafdsafdsa
                        (afdasffda
                         fdsafdsafdsaf fdsafdsaf fdsafdsafdsafdsa fdsafdsa) a)")

(multi-line-deftest-elisp test-checks-newlines-if-last-candidate
"(fdsafkldsfdsafdsafdsaf fdsafdsafdsaf fdsafdsafdsa (afdasffda
                                                    fdsafdsafdsaf fdsafdsaf fdsafdsafdsafdsa fdsafdsa))"
"(fdsafkldsfdsafdsafdsaf fdsafdsafdsaf fdsafdsafdsa
                        (afdasffda
                         fdsafdsafdsaf fdsafdsaf fdsafdsafdsafdsa fdsafdsa))")

(multi-line-deftest-elisp test-find-index-multi-lining
"
(cl-defun tile-get-next-strategy
    (&optional (current-strategy (or tile-current-strategy
                                     (car (last tile-strategies)))))
  (let ((current-index (--find-index (equal current-strategy it) tile-strategies)))
    (if current-index
        (nth (mod (1+ current-index) (length tile-strategies)) tile-strategies)
      (car tile-strategies))))"
"
(cl-defun tile-get-next-strategy
    (&optional (current-strategy (or tile-current-strategy
                                     (car (last tile-strategies)))))
  (let ((current-index (--find-index (equal current-strategy it)
                                     tile-strategies)))
    (if current-index
        (nth (mod (1+ current-index) (length tile-strategies)) tile-strategies)
      (car tile-strategies))))"
:setup ((search-forward "find-index")))

(provide 'multi-line-elisp-test)
;;; multi-line-elisp-test.el ends here
