;;; multi-line.el --- multi-line statements -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>
;; Keywords: multi line length whitespace programming
;; URL: https://github.com/IvanMalison/multi-line
;; Version: 0.0.0
;; Package-Requires: ((emacs "24"))

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

;; multi-line aims to provide a flexible framework for automatically
;; multi-lining and single-lining function invocations and
;; definitions, array and map literals and more. It relies on
;; functions that are defined on a per major mode basis wherever it
;; can so that it functions correctly across many different
;; programming languages.

;;; Code:

(require 'eieio)

(defun multi-line-lparenthesis-advance ()
  "Advance to the beginning of a statement that can be multi-lined."
  (re-search-forward "[[{(]"))

(defun multi-line-up-list-back ()
  "Go to the beginning of a statement from inside the statement."
  (interactive)
  (let ((string-start (nth 8 (syntax-ppss))))
    (when string-start
      (goto-char string-start)))
  (up-list) (backward-sexp))

(defclass multi-line-up-list-enter-strategy () nil)

(defmethod multi-line-enter ((enter multi-line-up-list-enter-strategy))
  (multi-line-up-list-back)
  (forward-char))

(defclass multi-line-forward-sexp-enter-strategy ()
  ((done-regex :initarg :done-regex :initform "[[:space:]]*[[({]")
   (advance-fn :initarg :advance-fn :initform 'multi-line-lparenthesis-advance)
   (inside-fn :initarg :inside-fn :initform 'multi-line-up-list-back)))

(defmethod multi-line-enter ((enter multi-line-forward-sexp-enter-strategy))
  (condition-case nil
      (let (last-point)
        (while (not (or (looking-at (oref enter :done-regex))
                        (equal last-point (point))))
          (setq last-point (point))
          (forward-sexp)))
    ('scan-error
     (funcall (oref enter :inside-fn))))
  (funcall (oref enter :advance-fn)))

(defun multi-line-comma-advance ()
  "Advance to the next comma."
  (re-search-forward ","))

(defclass multi-line-forward-sexp-find-strategy ()
  ((split-regex :initarg :split-regex :initform "[[:space:]]*,")
   (done-regex :initarg :done-regex :initform "[[:space:]]*[})]")
   (split-advance-fn :initarg :split-advance-fn :initform
                     'multi-line-comma-advance)))

(defmethod multi-line-should-stop ((strategy multi-line-forward-sexp-find-strategy))
  (cond
   ((looking-at (oref strategy :done-regex)) :done)
   ((looking-at (oref strategy :split-regex)) :candidate)
   (t nil)))

(defmethod multi-line-find-next ((strategy multi-line-forward-sexp-find-strategy))
  (let (last last-point this-point)
    (setq this-point (point))
    (condition-case nil
        (while (and (not (equal this-point last-point))
                           (not (setq last (multi-line-should-stop strategy))))
                 (forward-sexp)
                 (setq last-point this-point)
                 (setq this-point (point)))
      ('error (setq last :done))
      nil)
    (when (equal last :candidate) (funcall (oref strategy :split-advance-fn)))
    last))

(defclass multi-line-never-newline ()
  ((spacer :initarg :spacer :initform " ")))

(defmethod multi-line-respace ((respacer multi-line-never-newline) index markers)
  (when (not (or (equal 0 index)
                 (equal index (- (length markers) 1))))
        (insert (oref respacer :spacer))))

(defclass multi-line-always-newline ()
  ((always-first :initarg :skip-first :initform nil)
   (always-last :initarg :skip-last :initform nil)))

(defmethod multi-line-should-newline ((respacer multi-line-always-newline)
                                      index markers)
  (let ((marker-length (length markers)))
    (not (or (and (equal 0 index) (oref respacer :skip-first))
             (and (equal index (- marker-length 1)) (oref respacer :skip-last))))))

(defmethod multi-line-respace ((respacer multi-line-always-newline) index markers)
  (when (multi-line-should-newline respacer index markers)
    (newline-and-indent)))

(defclass multi-line-fill-respacer ()
  ((newline-at :initarg :newline-at :initform 80)
   (newline-respacer :initarg :newline-respacer :initform
                       (make-instance multi-line-always-newline))
   (default-respacer :initarg :default-respacer :initform
     (make-instance multi-line-never-newline))))

(defmethod multi-line-should-newline ((respacer multi-line-fill-respacer)
                                      index markers)
  (let ((marker-length (length markers)))
    (or (and (equal 0 index))
        (and (equal index (- marker-length 1)))
        (and (< (+ index 1) marker-length)
             (save-excursion
               (goto-char (marker-position (nth (+ index 1) markers )))
               (> (current-column) (oref respacer :newline-at)))))))

(defmethod multi-line-respace ((respacer multi-line-fill-respacer) index markers)
  (multi-line-respace
   (if (multi-line-should-newline respacer index markers)
       (oref respacer :newline-respacer)
     (oref respacer :default-respacer)) index markers))

(defclass multi-line-additional-action-decorator ()
  ((respacer :initarg :respacer :initform
             (make-instance multi-line-always-newline))
   (additional-action :initarg :additional-action)))

(defmethod multi-line-respace ((action multi-line-additional-action-decorator)
                               index markers)
  (multi-line-respace (oref action :respacer) index markers)
  (funcall (oref action :additional-action) index markers))

(defun multi-line-trailing-comma (index markers)
  "Add a trailing comma when at the last marker.

INDEX is the index that will be used to determine whether or not
the action should be taken.  MARKERS is the list of markers that
were generated for the statement."
  (when (equal index (- (length markers) 1))
    (re-search-backward "[^[:space:]\n]")
    (when (not (looking-at ","))
      (forward-char)
      (insert ","))))

(defun multi-line-trailing-comma-respacer (respacer)
  "Apply a comma adding multi-line-additional-action-decorator to RESPACER."
  (make-instance multi-line-additional-action-decorator :respacer respacer
                 :additional-action 'multi-line-trailing-comma))

(defun multi-line-get-markers (enter-strategy find-strategy)
  "Get the markers for multi-line candidates for the statement at point.

ENTER-STRATEGY is a class with the method multi-line-enter, and
FIND-STRATEGY is a class with the method multi-line-find-next."
  (multi-line-enter enter-strategy)
  (let ((markers (list (point-marker))))
    (nconc markers
          (cl-loop until (equal (multi-line-find-next find-strategy) :done)
                   collect (point-marker)))
    (nconc markers (list (point-marker)))))

(defun multi-line-clear-whitespace-at-point ()
  "Erase any surrounding whitespace."
  (interactive)
  (re-search-backward "[^[:space:]\n]")
  (forward-char)
  (let ((start (point)))
    (re-search-forward "[^[:space:]\n]")
    (backward-char)
    (kill-region start (point))))

(defclass multi-line-strategy ()
  ((enter :initarg :enter :initform
          (make-instance multi-line-up-list-enter-strategy))
   (find :initarg :find :initform
         (make-instance multi-line-forward-sexp-find-strategy))
   (respace :initarg :respace :initform
            (make-instance multi-line-always-newline))
   (sl-respace :initarg :sl-respace :initform
               (make-instance multi-line-never-newline))))

(defmethod multi-line-markers ((strategy multi-line-strategy))
  (multi-line-get-markers (oref strategy :enter) (oref strategy :find)))

(defmethod multi-line-execute ((strategy multi-line-strategy)
                               for-single-line)
  (let ((markers (multi-line-markers strategy))
        (respacer (if for-single-line (oref strategy :sl-respace)
                    (oref strategy :respace))))
    (cl-loop for marker being the elements of markers using (index i) do
             (multi-line-execute-one strategy marker i markers respacer))))

(defmethod multi-line-execute-one ((strategy multi-line-strategy)
                                   marker i markers respacer)
  (goto-char (marker-position marker))
  (multi-line-clear-whitespace-at-point)
  (multi-line-respace respacer i markers))

(defclass multi-line-major-mode-strategy-selector ()
  ((default-strategy :initarg :default-strategy :initform
     (make-instance multi-line-strategy))
   (strategy-map :initarg :strategy-map :initform (make-hash-table))))

(defmethod multi-line-execute ((selector multi-line-major-mode-strategy-selector)
                               for-single-line)
  (let ((strategy (or (gethash major-mode (oref selector :strategy-map))
                      (oref selector :default-strategy))))
    (multi-line-execute strategy for-single-line)))

(defmethod multi-line-set-strategy
  ((selector multi-line-major-mode-strategy-selector)
   for-mode strategy)

  (puthash for-mode strategy (oref selector :strategy-map)))

(defmethod multi-line-set-default-strategy
  ((selector multi-line-major-mode-strategy-selector) strategy)

  (oset selector :default-strategy strategy))

(defvar multi-line-master-strategy
      (make-instance multi-line-major-mode-strategy-selector))

(defun multi-line-lisp-advance-fn ()
  "Advance to the start of the next multi-line split for Lisp."
  (re-search-forward "[^[:space:]\n]")
  (backward-char))

(defvar multi-line-skip-respacer
  (make-instance multi-line-always-newline
                 :skip-first t :skip-last t))

(defvar multi-line-skip-fill-respacer
  (make-instance multi-line-fill-respacer
                 :newline-respacer multi-line-skip-respacer))

(defvar multi-line-skip-fill-stragety
  (make-instance multi-line-strategy
                 :respace multi-line-skip-fill-respacer))

(defvar multi-line-fill-stragety
  (make-instance multi-line-strategy
                 :respace (make-instance multi-line-fill-respacer)))

(defun multi-line-set-per-major-mode-strategies ()
  "Set language specific strategies."
  (interactive)

  (multi-line-set-strategy
   multi-line-master-strategy 'emacs-lisp-mode
   (make-instance multi-line-strategy
                  :find
                  (make-instance
                   multi-line-forward-sexp-find-strategy
                   :split-regex "[[:space:]\n]+"
                   :done-regex "[[:space:]]*)"
                   :split-advance-fn 'multi-line-lisp-advance-fn)
                  :enter
                  (make-instance
                   multi-line-up-list-enter-strategy)
                  :respace multi-line-skip-fill-respacer))

  (multi-line-set-strategy
   multi-line-master-strategy 'go-mode
   (make-instance multi-line-strategy
                  :respace
                  (multi-line-trailing-comma-respacer
                   (make-instance multi-line-fill-respacer)))))

(multi-line-set-per-major-mode-strategies)

;;;###autoload
(defun multi-line (arg)
  "Multi-line the statement at point.

When ARG is provided single-line the statement at point instead."
  (interactive "P")
  (let ((for-single-line (if arg t nil))) ; TODO(imalison): better cast to bool
    (multi-line-execute multi-line-master-strategy for-single-line)))

;;;###autoload
(defun multi-line-single-line ()
  "Single-line the statement at point."
  (interactive)
  (multi-line-execute multi-line-master-strategy t))

(provide 'multi-line)
;;; multi-line.el ends here
