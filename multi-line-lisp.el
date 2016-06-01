;;; multi-line-lisp.el --- multi-line statements -*- lexical-binding: t; -*-

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

;; multi-line-lisp defines a default configuration for lisp multi-lining

;;; Code:

(require 'multi-line-respace)
(require 'multi-line-shared)

(defun multi-line-lisp-newline-predicate (index markers fill-decider)
  (and (not (multi-line-is-newline-between-markers (nth markers 0) (nth markers index)))
       
       ))

(defun multi-line-make-lisp-respacer ()
  (make-instance multi-line-newline-predicate-respacer
                 :newline-predicate 'multi-line-lisp-newline-predicate))

(provide 'multi-line-lisp)
;;; multi-line-lisp.el ends here
