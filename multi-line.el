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

(defvar multi-line-default-single-line-respacer
  (multi-line-clearing-reindenting-respacer (multi-line-never-newline)))

(defvar multi-line-skip-first-and-last-respacer
  (make-instance multi-line-always-newline
                 :skip-first t :skip-last t))

(cl-defun multi-line-respacers-with-single-line
    (respacers
     &optional (single-line-respacer multi-line-default-single-line-respacer))
  (multi-line-build-from-respacers-list
   (append respacers (list (cons :single-line single-line-respacer)))))

(defun multi-line-default-respacers (&rest respacers)
  "Add a single-line strategy to RESPACERS and make a cycling respacer strategy."
  (multi-line-respacers-with-single-line respacers))

(defvar multi-line-skip-fill-respacer
  (multi-line-clearing-reindenting-respacer
   (multi-line-fill-column-respacer
    :newline-respacer multi-line-skip-first-and-last-respacer)))

(defvar multi-line-default-respacer-list
  (mapcar 'multi-line-clearing-reindenting-respacer
          (list (multi-line-fill-column-respacer)
                (multi-line-always-newline)
                (multi-line-fill-column-respacer
                 :newline-respacer multi-line-skip-first-and-last-respacer))))

(defvar multi-line-default-respacer
  (multi-line-respacers-with-single-line multi-line-default-respacer-list))

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

(defvar-local multi-line-current-strategy
  (make-instance multi-line-strategy)
  "The multi-line strategy that will be used by the command `multi-line'.")

(defun multi-line-lisp-advance-fn ()
  "Advance to the start of the next multi-line split for Lisp."
  (re-search-forward "[^[:space:]\n]")
  (backward-char))

(eval-and-compile
  (defvar multi-line-defhook-prefix "multi-line-"))

(defvar multi-line-mode-to-hook nil)

(defmacro multi-line-defhook
    (mode-name strategy-form &optional use-global-enable)
  (let* ((mode-string (symbol-name mode-name))
         (base-string (concat multi-line-defhook-prefix (symbol-name mode-name)))
         (variable-name (intern (concat base-string "-strategy")))
         (hook-name (intern (concat base-string "-mode-hook")))
         (mode-hook-name (intern (concat mode-string "-mode-hook"))))
    `(eval-and-compile
       (defvar ,variable-name)
       ;; Doing a setq separately lets use override any existing value
       (setq ,variable-name ,strategy-form)
       (defun ,hook-name ()
         (setq-local multi-line-current-strategy ,variable-name))
       ,(if use-global-enable
            `(add-to-list (quote multi-line-mode-to-hook)
                          (cons (quote ,mode-hook-name) (quote ,hook-name)))
          `(add-hook (quote ,mode-hook-name) (quote ,hook-name) t)))))

(put 'multi-line-defhook 'lisp-indent-function 1)

(defvar multi-line-lisp-respacer
  (multi-line-default-respacers multi-line-skip-fill-respacer))

(defvar multi-line-lisp-strategy
  (multi-line-strategy
   :find (multi-line-forward-sexp-find-strategy
          :split-regex "[[:space:]\n]+"
          :done-regex "[[:space:]]*)"
          :split-advance-fn 'multi-line-lisp-advance-fn)
   :enter (multi-line-up-list-enter-strategy)
   :respace multi-line-lisp-respacer) t)

(multi-line-defhook lisp multi-line-lisp-strategy t)

(multi-line-defhook emacs-lisp multi-line-lisp-strategy t)

(multi-line-defhook clojure
  (multi-line-strategy
   :find (multi-line-forward-sexp-find-strategy
          :split-regex "[[:space:]\n]+"
          :done-regex "[[:space:]]*)]}"
          :split-advance-fn 'multi-line-lisp-advance-fn)
   :enter (multi-line-up-list-enter-strategy)
   :respace multi-line-lisp-respacer) t)

(multi-line-defhook go
  (multi-line-strategy
   :respace (multi-line-respacers-with-single-line
             (mapcar 'multi-line-trailing-comma-respacer
                     multi-line-default-respacer-list)
             (multi-line-trailing-comma-respacer
              multi-line-default-single-line-respacer))) t)

;;;###autoload
(defun multi-line-enable-mode-hooks ()
  "Set default language specific strategies for multi-line."
  (interactive)
  (cl-loop for (target-name . hook-name) in multi-line-mode-to-hook
           do (add-hook target-name hook-name t)))

;;;###autoload
(defun multi-line-disable-mode-hooks ()
  "Remove default language specific strategies for multi-line."
  (interactive)
  (cl-loop for (target-name . hook-name) in multi-line-mode-to-hook
           do (remove-hook target-name hook-name)))

(multi-line-enable-mode-hooks)

;;;###autoload
(defun multi-line (arg)
  "Multi-line the statement at point.

When ARG is provided single-line the statement at point instead."
  (interactive "P")
  (let ((for-single-line (if arg t nil))) ; TODO(imalison): better cast to bool
    (multi-line-execute multi-line-current-strategy for-single-line)))

;;;###autoload
(defun multi-line-single-line ()
  "Single-line the statement at point."
  (interactive)
  (multi-line-execute multi-line-current-strategy t))

(provide 'multi-line)
;;; multi-line.el ends here
