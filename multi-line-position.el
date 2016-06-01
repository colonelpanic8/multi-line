;;; multi-line-position.el --- multi-line statements -*- lexical-binding: t; -*-

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

;; multi-line-position provides code to handle dealing with marker positions

;;; Code:

(require 'eieio)
(require 'multi-line-shared)

(defun multi-line-get-marker-column (index markers &optional additional-navigation)
  (save-excursion
    (goto-char (marker-position (nth markers index)))
    (when additional-navigation
      (funcall additional-navigation index markers))
    (current-column)))

(defclass multi-line-position-finder nil
  ((additional-navigation :initarg :additional-navigation
                          :initform nil)))

(defmethod multi-line-get-expected-final-column ((position multi-line-position-finder)
                                                 index markers)
  (multi-line-get-marker-column index markers
                                (oref position :additional-navigation)))

(provide 'multi-line-position)
;;; multi-line-position.el ends here
