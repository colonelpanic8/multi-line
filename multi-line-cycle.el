;;; multi-line-cycle.el --- multi-line statements -*- lexical-binding: t; -*-

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

;; multi-line-cycle defines a respacer object that dispatch to various
;; other respacers based on context and the previous user action.

;;; Code:

(require 'eieio)

;; This variable is for internal use only
(defvar multi-line-last-cycler nil)

(defclass multi-line-cycle-respacer ()
  ((respacers :initarg :respacers)
   (named-respacers :initarg :named-respacers :initform nil)
   (last-cycle-marker :initform nil)
   (cycle-index :initform 0)
   (command-at-last-cycle :initform nil)
   (check-markers :initform t)))

(defmethod multi-line-respace ((cycler multi-line-cycle-respacer) markers
                               &optional context)
  (let* ((respacer-name (plist-get context :respacer-name))
         (respacer-index (plist-get context :respacer-index))
         (respacer
          (or (plist-get (oref cycler named-respacers)
                                 respacer-name)
              (when respacer-index
                (nth respacer-index (oref cycler respacers))))))
    (if respacer
        (multi-line-cycler-reset cycler)
      (setq respacer (multi-line-cycle cycler)))
    (multi-line-respace respacer markers context)))

(defmethod multi-line-cycle ((cycler multi-line-cycle-respacer))
  (if (and (eq multi-line-last-cycler cycler)
             (equal (oref cycler command-at-last-cycle) this-command)
             (if (oref cycler check-markers)
                 (let ((current-marker (point-marker))
                       (last-marker (oref cycler last-cycle-marker)))
                  (equal current-marker last-marker))
               t))
                   ;; Because the respace phase occurs AFTER markers
                   ;; are obtained, but before the end of the
                   ;; save-excursion in the execute call, and the
                   ;; function to obtain the markers moves the cursor
                   ;; to a final position in a deterministic way, this
                   ;; condition will actually allow the user to move
                   ;; their cursor within the multi-space body and
                   ;; still get cycling behavior.
      (multi-line-increment-cycle-index cycler)
    (multi-line-cycler-reset cycler))
  (multi-line-current-respacer cycler))

(defmethod multi-line-current-respacer ((cycler multi-line-cycle-respacer))
  (nth (oref cycler cycle-index) (oref cycler respacers)))

(defmethod multi-line-cycler-reset ((cycler multi-line-cycle-respacer))
  (oset cycler last-cycle-marker (point-marker))
  (oset cycler cycle-index 0)
  (oset cycler command-at-last-cycle this-command)
  (setq multi-line-last-cycler cycler))

(defmethod multi-line-increment-cycle-index ((cycler multi-line-cycle-respacer)
                                             &optional amount)
  (unless amount (setq amount 1))
  (oset cycler cycle-index
        (% (+ (oref cycler cycle-index) amount)
           (length (oref cycler respacers)))))

(defun multi-line-build-from-respacers-list (respacers-list)
  (let* ((named-respacers nil)
         (respacers
          (cl-loop for respacer-spec in respacers-list
                   collect (pcase respacer-spec
                     (`(,name . ,respacer)
                      (setq named-respacers
                            (plist-put named-respacers name respacer))
                      respacer)
                     (_ respacer-spec)))))
    (multi-line-cycle-respacer :respacers respacers
                               :named-respacers named-respacers)))

(provide 'multi-line-cycle)
;;; multi-line-cycle.el ends here
