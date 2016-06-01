;;; multi-line-respace.el --- multi-line statements -*- lexical-binding: t; -*-

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

;; multi-line-respace defines various generally applicable respace
;; strategies.

;;; Code:

(require 'eieio)
(require 'multi-line-position)

(defclass multi-line-respacer () nil)

(defmethod multi-line-respace ((respacer multi-line-respacer) markers)
  (cl-loop for marker being the elements of markers using (index i) do
           (goto-char (marker-position marker))
           (multi-line-respace-one respacer i markers)))

(defclass multi-line-never-newline (multi-line-respacer)
  ((spacer :initarg :spacer :initform " ")))

(defmethod multi-line-respace-one ((respacer multi-line-never-newline) index markers)
  (when (not (or (equal 0 index)
                 (equal index (- (length markers) 1))
                 (multi-line-spacer-at-point respacer)))
    (insert (oref respacer :spacer))))

(defmethod multi-line-spacer-at-point ((respacer multi-line-never-newline))
  (save-excursion (re-search-backward (format "[^%s]" (oref respacer :spacer)))
                  (forward-char)
                  (looking-at (oref respacer :spacer))))

(defclass multi-line-always-newline (multi-line-respacer)
  ())

(defmethod multi-line-respace-one ((_respacer multi-line-always-newline)
                                   _index _markers)
  (when (not (looking-at "[[:space:]]*\n"))
    (newline-and-indent)))

(defclass multi-line-newline-predicate-respacer (multi-line-respacer)
  ((newline-respacer :initarg :newline-respacer :initform
                     (make-instance multi-line-always-newline))
   (default-respacer :initarg :default-respacer :initform
     (make-instance multi-line-never-newline))
   (newline-predicate :initarg :newline-predicate)))

(defmethod multi-line-respace-one ((respacer multi-line-newline-predicate-respacer)
                                   index markers)
  (multi-line-respace-one
   (if (multi-line-should-newline respacer index markers)
       (oref respacer :newline-respacer)
     (oref respacer :default-respacer)) index markers))

(defmethod multi-line-should-newline ((respacer multi-line-newline-predicate-respacer)
                                      index markers)
  (funcall (oref respacer :newline-predicate) index markers))

(defclass multi-line-fill-decider nil
  ((position :initarg :position
             :initform (make-instance multi-line-position-finder))))

(defmethod multi-line-should-newline ((respacer multi-line-fill-decider)
                                      index markers)
  (let ((marker-length (length markers)))
    (and (< (+ index 1) marker-length)
             (save-excursion
               (goto-char (marker-position (nth (+ index 1) markers)))
               (> (current-column) (multi-line-get-fill-column respacer))))))

(defmethod multi-line-markers-will-require-newline ((decider multi-line-fill-decider)
                                                    markers)
  (save-excursion
    (goto-char (marker-position (nth markers (- (length markers) 1))))
    (> (current-column) )
    ))

(defclass multi-line-fixed-fill-decider (multi-line-fill-decider)
  ((newline-at :initarg :newline-at :initform 80)))

(defmethod multi-line-get-fill-column ((respacer multi-line-fixed-fill-decider))
  (oref respacer :newline-at))

(defclass multi-line-fill-column-decider (multi-line-fill-decider) nil)

(defmethod multi-line-get-fill-column ((_respacer multi-line-fill-column-decider))
  fill-column)

(provide 'multi-line-respace)
;;; multi-line-respace.el ends here
