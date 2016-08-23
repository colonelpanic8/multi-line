;;; multi-line.el --- multi-line statements -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2016 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>
;; Keywords: multi line length whitespace programming
;; URL: https://github.com/IvanMalison/multi-line
;; Version: 0.0.0
;; Package-Requires: ((emacs "24") (s "1.9.0") (cl-lib "0.5"))

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
;; can so that it behaves correctly across many different programming
;; languages.

;;; Code:

(require 'cl-lib)
(require 'eieio)

(require 'multi-line-cycle)
(require 'multi-line-decorator)
(require 'multi-line-enter)
(require 'multi-line-find)
(require 'multi-line-respace)
(require 'multi-line-shared)

(cl-defun multi-line-respacers-with-single-line
    (respacers
     &optional (single-line-respacer multi-line-default-single-line-respacer))
  (multi-line-build-from-respacers-list
   (nconc respacers (list (cons :single-line single-line-respacer)))))

(defun multi-line-default-respacers (&rest respacers)
  (multi-line-respacers-with-single-line respacers))

(defvar multi-line-default-single-line-respacer
  (multi-line-clearing-reindenting-respacer (multi-line-never-newline)))

(defvar multi-line-skip-first-and-last-respacer
  (make-instance multi-line-always-newline
                 :skip-first t :skip-last t))

(defvar multi-line-skip-fill-respacer
  (multi-line-clearing-reindenting-respacer
   (multi-line-fill-column-respacer
    :newline-respacer multi-line-skip-first-and-last-respacer)))

(defvar multi-line-default-respacer
  (multi-line-respacers-with-single-line
             (mapcar 'multi-line-clearing-reindenting-respacer
                     (list (multi-line-fill-column-respacer)
                           (multi-line-always-newline)
                           (multi-line-fill-column-respacer
                            :newline-respacer multi-line-skip-first-and-last-respacer)))))

(defclass multi-line-strategy ()
  ((enter :initarg :enter :initform
          (make-instance multi-line-up-list-enter-strategy))
   (find :initarg :find :initform
         (make-instance multi-line-forward-sexp-find-strategy))
   (respace :initarg :respace :initform (progn
                                          multi-line-default-respacer))))

(defmethod multi-line-markers ((strategy multi-line-strategy) &optional context)
  "Get the markers for multi-line candidates for the statement at point."
  (let ((enter-strategy (oref strategy :enter))
        (find-strategy (oref strategy :find)))
    (multi-line-enter enter-strategy context)
    ;; TODO: This logic should probably be part of the find-strategy
    (nconc (list (point-marker))        ;start marker
           (cl-loop until (equal (multi-line-find-next find-strategy context) :done)
                    collect (point-marker))
           (list (point-marker))        ;end marker
           )))

(defmethod multi-line-execute ((strategy multi-line-strategy) &optional context)
  (when (or (eq context t) (equal context 'single-line))
    (setq context (plist-put nil :respacer-name :single-line)))
  (save-excursion
    (let ((markers (multi-line-markers strategy)))
      (multi-line-respace (oref strategy :respace) markers context))))

(defmethod multi-line-execute-one ((strategy multi-line-strategy)
                                   marker i markers respacer)
  (goto-char (marker-position marker))
  (multi-line-respace-one respacer i markers))

(defclass multi-line-major-mode-strategy-selector ()
  ((default-strategy :initarg :default-strategy :initform
     (make-instance multi-line-strategy))
   (strategy-map :initarg :strategy-map :initform (make-hash-table))))

(defmethod multi-line-execute ((selector multi-line-major-mode-strategy-selector)
                               context)
  (let ((strategy (or (gethash major-mode (oref selector :strategy-map))
                      (oref selector :default-strategy))))
    (multi-line-execute strategy context)))

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

(defun multi-line-set-per-major-mode-strategies ()
  "Set language specific strategies for multi-line."
  (interactive)
  (multi-line-set-strategy
   multi-line-master-strategy 'emacs-lisp-mode
   (multi-line-strategy
    :find (multi-line-forward-sexp-find-strategy
           :split-regex "[[:space:]\n]+"
           :done-regex "[[:space:]]*)"
           :split-advance-fn 'multi-line-lisp-advance-fn)
    :enter (multi-line-up-list-enter-strategy)
    :respace (multi-line-default-respacers multi-line-skip-fill-respacer)))

  (multi-line-set-strategy
   multi-line-master-strategy 'clojure-mode
   (multi-line-strategy
    :find (multi-line-forward-sexp-find-strategy
           :split-regex "[[:space:]\n]+"
           :done-regex "[[:space:]]*)}]"
           :split-advance-fn 'multi-line-lisp-advance-fn)
    :enter (multi-line-up-list-enter-strategy)
    :respace (multi-line-default-respacers multi-line-skip-fill-respacer)))

  (multi-line-set-strategy
   multi-line-master-strategy 'go-mode
   (multi-line-strategy
    :respace (multi-line-default-respacers
              (multi-line-trailing-comma-respacer
               (multi-line-fixed-fill-respacer))))))

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
