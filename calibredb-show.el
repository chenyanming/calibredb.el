;;; calibredb-show.el --- Book detail buffer for calibredb -*- lexical-binding: t; -*-

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

(declare-function calibredb-entry-dispatch "calibredb-transient.el")
(declare-function calibredb-find-file "calibredb-utils.el")
(declare-function calibredb-find-file-other-frame "calibredb-utils.el")
(declare-function calibredb-open-file-with-default-tool "calibredb-utils.el")
(declare-function calibredb-set-metadata-dispatch "calibredb-transient.el")
(declare-function calibredb-export-dispatch "calibredb-transient.el")
(declare-function calibredb-open-dired "calibredb-utils.el")
(declare-function calibredb-set-metadata--tags "calibredb-utils.el")
(declare-function calibredb-set-metadata--author_sort "calibredb-utils.el")
(declare-function calibredb-set-metadata--authors "calibredb-utils.el")
(declare-function calibredb-set-metadata--title "calibredb-utils.el")
(declare-function calibredb-set-metadata--comments "calibredb-utils.el")
(declare-function calibredb-show-entry "calibredb-search.el")
(declare-function calibredb-rga "calibredb-ivy.el")

(defvar calibredb-show-entry nil
  "The entry being displayed in this buffer.")

(defvar calibredb-show-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" #'calibredb-entry-dispatch)
    (define-key map "o" #'calibredb-find-file)
    (define-key map "O" #'calibredb-find-file-other-frame)
    (define-key map "V" #'calibredb-open-file-with-default-tool)
    (define-key map "s" #'calibredb-set-metadata-dispatch)
    (define-key map "e" #'calibredb-export-dispatch)
    (define-key map "q" #'calibredb-entry-quit)
    (define-key map "y" #'calibredb-yank-dispatch)
    (define-key map "," #'calibredb-quick-look)
    (define-key map "." #'calibredb-open-dired)
    (define-key map "\M-/" #'calibredb-rga)
    (define-key map "\M-t" #'calibredb-set-metadata--tags)
    (define-key map "\M-a" #'calibredb-set-metadata--author_sort)
    (define-key map "\M-A" #'calibredb-set-metadata--authors)
    (define-key map "\M-T" #'calibredb-set-metadata--title)
    (define-key map "\M-c" #'calibredb-set-metadata--comments)
    map)
  "Keymap for `calibredb-show-mode'.")

(defcustom calibredb-show-unique-buffers nil
  "TODO: When non-nil, every entry buffer gets a unique name.
This allows for displaying multiple show buffers at the same
time."
  :group 'calibredb
  :type 'boolean)


(defcustom calibredb-show-entry-switch #'switch-to-buffer-other-window
  "Function used to display the calibre entry buffer."
  :group 'calibredb
  :type '(choice (function-item switch-to-buffer-other-window)
                 (function-item switch-to-buffer)
                 (function-item pop-to-buffer)
                 function))

(define-derived-mode calibredb-show-mode fundamental-mode "calibredb-show"
  "Mode for displaying book entry details.
\\{calibredb-show-mode-map}"
  (setq buffer-read-only t)
  (buffer-disable-undo))

(defun calibredb-show--buffer-name (entry)
  "Return the appropriate buffer name for ENTRY.
The result depends on the value of `calibredb-show-unique-buffers'."
  (if calibredb-show-unique-buffers
      (format "*calibredb-entry-<%s>*"
              (calibredb-getattr entry :book-title))
    "*calibredb-entry*"))

(defun calibredb-show-refresh ()
  "Refresh ENTRY in the current buffer."
  (interactive)
  (let* ((entry (get-text-property (point-min) 'calibredb-entry nil)) ; old entry
         (id (calibredb-getattr entry :id)) ; only get the id
         (query-result (cdr (car (calibredb-candidate id))))) ; get the new entry through SQL query
    (calibredb-show-entry query-result)))

(defun calibredb-entry-quit ()
  "Quit the *calibredb-entry*."
  (interactive)
  (when (eq major-mode 'calibredb-show-mode)
    (when (get-buffer "*calibredb-entry*")
      (pop-to-buffer "*calibredb-entry*")
      (if (< (length (window-prev-buffers)) 2)
        (progn
          (quit-window)
          (kill-buffer "*calibredb-entry*"))
        (kill-buffer "*calibredb-entry*")))))

(provide 'calibredb-show)

;;; calibredb-show.el ends here
