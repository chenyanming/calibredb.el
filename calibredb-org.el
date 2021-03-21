;;; calibredb-org.el --- Org mode supported features for calibredb -*- lexical-binding: t; -*-

;; Author: Damon Chan <elecming@gmail.com>

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
(require 'ol)

(declare-function calibredb-show-entry "calibredb-search.el")
(declare-function calibredb-find-marked-candidates "calibredb-utils.el")
(declare-function calibredb-find-candidate-at-point "calibredb-utils.el")


(defun calibredb-org-link-view (id _)
  "Follow calibredb org links."
  (calibredb-show-entry (cdar (calibredb-candidate id))))

(org-link-set-parameters "calibredb" :follow #'calibredb-org-link-view)

(defun calibredb-org-link-copy ()
  "Copy the marked items as calibredb org links."
  (interactive)
  (let ((candidates (calibredb-find-marked-candidates)))
    (unless candidates
      (setq candidates (calibredb-find-candidate-at-point)))
    (kill-new
     (with-temp-buffer
       (dolist (cand candidates)
         (let ((id (calibredb-getattr cand :id))
               (path (calibredb-getattr cand :file-path))
               (title (calibredb-getattr cand :book-title)))
           (insert (format "[[calibredb:%s][%s %s - %s]]\n"
                           id
                           (cond (calibredb-format-all-the-icons
                                  (if (fboundp 'all-the-icons-icon-for-file)
                                      (all-the-icons-icon-for-file path) ""))
                                 (calibredb-format-icons-in-terminal
                                  (if (fboundp 'icons-in-terminal-icon-for-file)
                                      (icons-in-terminal-icon-for-file path :v-adjust 0 :height 1) ""))
                                 (t "")) id title))
           (message "Copied: %s - \"%s\" as calibredb org link." id title)))
       (buffer-string)))
    ;; remove overlays and text properties
    (let* ((beg (point-min))
           (end (point-max))
           (inhibit-read-only t))
      (remove-overlays beg end)
      (remove-text-properties beg end '(calibredb-mark nil)))))

(provide 'calibredb-org)
