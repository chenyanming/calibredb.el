;;; calibredb-org.el --- Org mode supported features for calibredb -*- lexical-binding: t; -*-

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
(require 'ol)

(declare-function calibredb-show-entry "calibredb-search.el")
(declare-function calibredb-find-marked-candidates "calibredb-utils.el")
(declare-function calibredb-find-candidate-at-point "calibredb-utils.el")

;;;###autoload
(defun calibredb-org-link-view (id _)
  "Follow calibredb org links by ID."
  (calibredb-show-entry (cdar (calibredb-candidate id))))

;;;###autoload
(defun calibredb-org-complete-link (&optional prefix)
  "Define completion for Org \"calibredb:\" links.
The optional PREFIX argument is ignored.
Please notice: `calibredb-id-width' must >= the real id lenth."
  (ignore prefix)
  (let* ((candidates (if calibredb-search-entries
                   calibredb-search-entries
                 (progn
                   (setq calibredb-search-entries (calibredb-candidates))
                   (setq calibredb-full-entries calibredb-search-entries)))))
    (if (fboundp 'consult--read)
        (if candidates
            (let* ((cand (consult--read candidates
                                        :prompt "Pick a book: "
                                        :lookup #'consult--lookup-cdr
                                        :sort nil))
                   (id (cadr (assoc :id (car cand )) )))
              (concat (format "calibredb:%s" id)))
          "calibredb:")
      (if candidates
          (let* ((cand (completing-read "Pick a book: " candidates))
                 (id-point (text-property-not-all 0 (length cand) 'id nil cand))
                 (id (get-text-property id-point 'id cand)))
            (concat (format "calibredb:%s" id)))
        "calibredb:"))))


(defun calibredb-org-image-data-fun (_protocol id _description)
  "Get corresponding book ID cover page data.
Display cover page inline in org buffer. Use this as
:image-data-fun property in `org-link-properties'. See
`org-display-user-inline-images' for a description of
:image-data-fun."
  (if (string-match "[0-9]+" id)
      (with-current-buffer (find-file-noselect (calibredb-get-cover (cdar (calibredb-candidate id))))
        (buffer-substring-no-properties (point) (point-max)))))


;; `org-display-user-inline-images' is from package `org-yt'
(if (require 'org-yt nil 'noerror)
    (org-link-set-parameters
     "calibredb"
     :follow #'calibredb-org-link-view
     :complete #'calibredb-org-complete-link
     :image-data-fun #'calibredb-org-image-data-fun)
  (org-link-set-parameters
   "calibredb"
   :follow #'calibredb-org-link-view
   :complete #'calibredb-org-complete-link))


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
               (title (calibredb-getattr cand :book-title)))
           (insert (format "[[calibredb:%s][%s]]\n" id title))
           (message "Copied: %s - \"%s\" as calibredb org link." id title)))
       (buffer-string)))
    ;; remove overlays and text properties
    (let* ((beg (point-min))
           (end (point-max))
           (inhibit-read-only t))
      (remove-overlays beg end)
      (remove-text-properties beg end '(calibredb-mark nil)))))

(provide 'calibredb-org)

;;; calibredb-org.el ends here
