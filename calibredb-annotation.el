;;; calibredb-annotation.el --- Annotation for calibredb -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Damon Chan

;; Author: Damon Chan <elecming@gmail.com>
;; URL: https://github.com/chenyanming/calibredb.el
;; Keywords: tools
;; Version: 2.12.0

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'calibredb-core)

(declare-function calibredb-get-init "calibredb-utils.el")
(declare-function calibredb-find-candidate-at-point "calibredb-utils.el")
(declare-function calibredb-read-metadatas "calibredb-utils.el")
(declare-function calibredb-search-refresh-or-resume "calibredb-search.el")

(defvar calibredb-annotation-candidate nil
  "Local variable used in *calibredb-annotation*.")
(defvar calibredb-annotation-parameter nil
  "Local variable used in *calibredb-annotation*.")

(defvar calibredb-edit-annotation-header-function #'calibredb-edit-annotation-header
  "Function that returns the string to be used for the Calibredb edit annotation header.")

(defvar calibredb-edit-annotation-text-func nil
  "Function to return default text to use for an ebook annotation.
It takes one argument, the title of the ebook, as a string.
It could be function `calibredb-default-annotation-text'")

;; annotation

(define-derived-mode calibredb-edit-annotation-mode org-mode "calibredb-edit-annatation"
  "Mode for editing the annotation of a ebook.
When you have finished composing, use `C-c C-c'.
\\{calibredb-edit-annotation-mode-map}"
  (setq header-line-format '(:eval (funcall calibredb-edit-annotation-header-function))))

(defun calibredb-edit-annotation-header ()
  "TODO: Return the string to be used as the Calibredb edit annotation header."
  (format "%s -> Edit Annotation. %s %s"
          (propertize (calibredb-get-init "title" calibredb-annotation-candidate) 'face 'calibredb-edit-annotation-header-title-face)
           "Finish 'C-c C-c',"
           "abort 'C-c C-k'."))

(defun calibredb-edit-annotation (&optional candidate)
  "Pop up a buffer for editing ebook CANDIDATE's annotation."
  (interactive)
  (let (beg pos)
    ;; save the original position temporary.
    (setq beg (point))
    (setq pos (window-start))
    (unless candidate
      (setq candidate (car (calibredb-find-candidate-at-point))))
    (pop-to-buffer (generate-new-buffer-name "*calibredb-edit-annatation*"))
    (calibredb-insert-annotation candidate)
    (calibredb-edit-annotation-mode)
    (set (make-local-variable 'calibredb-annotation-candidate) candidate)
    (set (make-local-variable 'calibredb-annotation-parameter) `(,beg . ,pos))))

(defun calibredb-default-annotation-text (title)
  "Return default annotation text for TITLE.
The default annotation text is simply some text explaining how to use
annotations."
  (concat "#  Type the annotation for item `" title "' here.\n"
          "#  All lines that start with a `#' will be deleted.\n"
          "#  Type `C-c C-c' when done.\n#\n"
          "#  Author: " (user-full-name) " <" (user-login-name) "@"
          (system-name) ">\n"
          "#  Date:    " (current-time-string) "\n"))

(defun calibredb-insert-annotation (candidate)
  "Insert annotation for CANDIDATE."
  (when calibredb-edit-annotation-text-func
    (insert (funcall calibredb-edit-annotation-text-func
                     (calibredb-read-metadatas "title" candidate))))
  (let ((annotation  (calibredb-read-metadatas "comments" candidate)))
    (when (and annotation  (not (string-equal annotation ""))) (insert annotation))))

(defun calibredb-kill-line (&optional newline-too)
  "Kill from point to end of line.
If optional arg NEWLINE-TOO is non-nil, delete the newline too.
Does not affect the kill ring."
  (let ((eol (line-end-position)))
    (delete-region (point) eol)
    (when (and newline-too (= (following-char) ?\n))
      (delete-char 1))))

(defun calibredb-send-edited-annotation ()
  "Use buffer contents as annotation for an ebook.
Lines beginning with `#' are ignored.
Bound to \\<C-cC-c> in `calibredb-edit-annotation-mode'."
  (interactive)
  (unless (derived-mode-p 'calibredb-edit-annotation-mode)
    (error "Not in mode derived from `calibredb-edit-annotation-mode'"))
  (goto-char (point-min))
  ;; (while (< (point) (point-max)) (if (= (following-char) ?#) (calibredb-kill-line t) (forward-line 1)))
  (let ((annotation      (buffer-substring-no-properties (point-min) (point-max)))
        (candidate        calibredb-annotation-candidate)
        (beg        (car calibredb-annotation-parameter))
        (pos        (cdr calibredb-annotation-parameter)))
    (when (string= annotation "") (setq annotation nil))
    (calibredb-command :command "set_metadata"
                       :option (format "--field %s:%s " calibredb-annotation-field (prin1-to-string annotation))
                       :id (calibredb-getattr candidate :id)
                       :library (format "--library-path \"%s\"" calibredb-root-dir))
    (calibredb-annotation-quit)
    (calibredb-search-refresh-or-resume beg pos)))

(defun calibredb-annotation-quit ()
  "Quit *calibredb-edit-annatation*.
Bound to \\<C-cC-k> in `calibredb-edit-annotation-mode'."
  (interactive)
  (when (eq major-mode 'calibredb-edit-annotation-mode)
    (if (< (length (window-prev-buffers)) 2)
        (progn
          (quit-window)
          (kill-buffer "*calibredb-edit-annatation*"))
      (kill-buffer))))

(provide 'calibredb-annotation)

;;; calibredb-annotation.el ends here
