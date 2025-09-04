;;; calibredb-org.el --- Org mode supported features for calibredb -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Damon Chan

;; Author: Damon Chan <elecming@gmail.com>
;; URL: https://github.com/chenyanming/calibredb.el
;; Keywords: tools
;; Version: 2.13.0

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
  (let* ((candidates (calibredb-candidates)))
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
         (let* ((id (calibredb-getattr cand :id))
                (title (calibredb-getattr cand :book-title))
                (link (format "[[calibredb:%s][%s]]\n" id title)))
           (insert link)
           (message "Copied (org link): %s" link)))
       (buffer-string)))
    ;; remove overlays and text properties
    (let* ((beg (point-min))
           (end (point-max))
           (inhibit-read-only t))
      (remove-overlays beg end)
      (remove-text-properties beg end '(calibredb-mark nil)))))

(defun calibredb-org-title-copy ()
  "Copy the marked items' titles."
  (interactive)
  (let ((candidates (calibredb-find-marked-candidates)))
    (unless candidates
      (setq candidates (calibredb-find-candidate-at-point)))
    (kill-new
     (with-temp-buffer
       (dolist (cand candidates)
         (let* ((title (calibredb-getattr cand :book-title)))
           (insert title (if (> (length candidates) 1) "\n" ""))
           (message "Copied (title): %s" title)))
       (buffer-string)))
    ;; remove overlays and text properties
    (let* ((beg (point-min))
           (end (point-max))
           (inhibit-read-only t))
      (remove-overlays beg end)
      (remove-text-properties beg end '(calibredb-mark nil)))))

(defun calibredb-org-protocol-link-copy ()
  "Copy the marked items as org-protocol links."
  (interactive)
  (let ((candidates (calibredb-find-marked-candidates)))
    (unless candidates
      (setq candidates (calibredb-find-candidate-at-point)))
    (kill-new
     (with-temp-buffer
       (dolist (cand candidates)
         (let* ((id (calibredb-getattr cand :id))
                (title (calibredb-getattr cand :book-title))
                (org-protocol-link (url-encode-url (format "org-protocol://calibredb?id=%s&title=%s" id (url-hexify-string title)) ) ))
           ;; (insert (format "[[calibredb:%s][%s]]\n" id title))
           (insert org-protocol-link (if (> (length candidates) 1) "\n" ""))
           (message "Copied (org-protocol): %s" org-protocol-link)))
       (buffer-string)))
    ;; remove overlays and text properties
    (let* ((beg (point-min))
           (end (point-max))
           (inhibit-read-only t))
      (remove-overlays beg end)
      (remove-text-properties beg end '(calibredb-mark nil)))))


(defun calibredb-org-protocol-link-markdown-copy ()
  "Copy the marked items as org-protocol markdown links."
  (interactive)
  (let ((candidates (calibredb-find-marked-candidates)))
    (unless candidates
      (setq candidates (calibredb-find-candidate-at-point)))
    (kill-new
     (with-temp-buffer
       (dolist (cand candidates)
         (let* ((id (calibredb-getattr cand :id))
                (title (calibredb-getattr cand :book-title))
                (org-protocol-link (format "[%s](%s)" title (url-encode-url (format "org-protocol://calibredb?id=%s&title=%s" id (url-hexify-string title)) ))  ))
           (insert org-protocol-link (if (> (length candidates) 1) "\n" ""))
           (message "Copied (org-protocol markdown): %s" org-protocol-link)))
       (buffer-string)))
    ;; remove overlays and text properties
    (let* ((beg (point-min))
           (end (point-max))
           (inhibit-read-only t))
      (remove-overlays beg end)
      (remove-text-properties beg end '(calibredb-mark nil)))))

(defun calibredb-org-markdown-copy ()
  "Copy the marked items as markdown links."
  (interactive)
  (let ((candidates (calibredb-find-marked-candidates)))
    (unless candidates
      (setq candidates (calibredb-find-candidate-at-point)))
    (kill-new
     (with-temp-buffer
         (dolist (cand candidates)
           (let* ((id (calibredb-getattr cand :id))
                  (path (calibredb-get-file-path cand t))
                  (title (calibredb-getattr cand :book-title))
                  (org-protocol-link (format "[%s](%s)" title path)))
           (insert org-protocol-link (if (> (length candidates) 1) "\n" ""))
           (message "Copied (markdown): %s" org-protocol-link)))
         (buffer-string)))
    ;; remove overlays and text properties
    (let* ((beg (point-min))
           (end (point-max))
           (inhibit-read-only t))
      (remove-overlays beg end)
      (remove-text-properties beg end '(calibredb-mark nil)))))

(defun calibredb-org-protocol (data)
  (let* ((id (plist-get data :id))
         (title (plist-get data :title)))
    (calibredb-show-entry
     (cond ((and (file-exists-p (expand-file-name ".metadata.calibre" calibredb-root-dir)))
            (cdar (calibredb-folder-candidate id)))
           (t (cdar (calibredb-candidate id)))))
    nil))


(defun calibredb-org-setup-org-protocol()
  (require 'org-protocol)
  (add-to-list 'org-protocol-protocol-alist '("calibredb"
                                              :protocol "calibredb"
                                              :function calibredb-org-protocol
                                              :kill-client t)))

(provide 'calibredb-org)

;;; calibredb-org.el ends here
