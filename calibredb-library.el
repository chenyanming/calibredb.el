;;; calibredb-library.el --- Library for calibredb -*- lexical-binding: t; -*-

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

(defvar calibredb-library-index 0)
(defvar calibredb-virtual-library-index 0)

(declare-function calibredb-ref-default-bibliography "calibredb-utils.el")
(declare-function calibredb-search-refresh-or-resume "calibredb-search.el")
(declare-function calibredb-search-keyword-filter "calibredb-search.el")
(declare-function calibredb-opds-request-page "calibredb-opds.el")


;;;###autoload
(defun calibredb-switch-library ()
  "Swich Calibre Library."
  (interactive)
  (let* ((result (read-file-name "Quick switch library: "))
         (db (concat (file-name-as-directory result) "metadata.db")))
    (if (file-exists-p db)
        (progn
          (setq calibredb-root-dir result)
          (calibredb-root-dir-quote)
          (setq calibredb-db-dir (concat (file-name-as-directory calibredb-root-dir) "metadata.db"))
          (calibredb-ref-default-bibliography)
          (when (and
                 (functionp 'sqlite-available-p)
                 (sqlite-available-p)
                 (sqlitep calibredb-db-connection))
            (funcall 'sqlite-close calibredb-db-connection)
            (setq calibredb-db-connection nil))
          (calibredb-search-refresh-or-resume))
      (message "%s does not exists" db))))

;;;###autoload
(defun calibredb-library-list ()
  "Switch library from variable `calibredb-library-alist'.
If under *calibredb-search* buffer, it will auto refresh after
selecting the new item."
  (interactive)
  (let* ((result (completing-read "Quick switch library: " calibredb-library-alist))
         (db (concat (file-name-as-directory result) "metadata.db")))
    (if (file-exists-p db)
        (progn
          (setq calibredb-root-dir result)
          (calibredb-root-dir-quote)
          (setq calibredb-db-dir (concat (file-name-as-directory calibredb-root-dir) "metadata.db"))
          (calibredb-ref-default-bibliography)
          (when (and
                 (functionp 'sqlite-available-p)
                 (sqlite-available-p)
                 (sqlitep calibredb-db-connection))
            (funcall 'sqlite-close calibredb-db-connection)
            (setq calibredb-db-connection nil))
          (calibredb-search-refresh-or-resume))
      (if (s-contains? "http" result)
          (let ((library (-first (lambda (lib)
                                   (s-contains? (car lib) result))
                                 calibredb-library-alist)))
            (setq calibredb-root-dir (car library))
            (calibredb-opds-request-page result (nth 1 library) (nth 2 library)))
        (message "INVALID LIBRARY")))))

(defun calibredb-library-previous ()
  "Next library from variable `calibredb-library-alist'.
If under *calibredb-search* buffer, it will auto refresh after
selecting the new item."
  (interactive)
  (let* ((index (setq calibredb-library-index (if (> calibredb-library-index 0)
                                                  (1- calibredb-library-index)
                                                (1- (length calibredb-library-alist)))))
         (result (car (nth index calibredb-library-alist)))
         (db (concat (file-name-as-directory result) "metadata.db")))
    (if (file-exists-p db)
        (progn
          (setq calibredb-root-dir result)
          (calibredb-root-dir-quote)
          (setq calibredb-db-dir (concat (file-name-as-directory calibredb-root-dir) "metadata.db"))
          (calibredb-ref-default-bibliography)
          (when (and
                 (functionp 'sqlite-available-p)
                 (sqlite-available-p)
                 (sqlitep calibredb-db-connection))
            (funcall 'sqlite-close calibredb-db-connection)
            (setq calibredb-db-connection nil))
          (calibredb-search-refresh-or-resume))
      (message "%s does not exists" db))))

(defun calibredb-library-next ()
  "Next library from variable `calibredb-library-alist'.
If under *calibredb-search* buffer, it will auto refresh after
selecting the new item."
  (interactive)
  (let* ((index (setq calibredb-library-index (if (< calibredb-library-index (1- (length calibredb-library-alist)))
                                                  (1+ calibredb-library-index) 0)))
         (result (car (nth index calibredb-library-alist)))
         (db (concat (file-name-as-directory result) "metadata.db")))
    (if (file-exists-p db)
        (progn
          (setq calibredb-root-dir result)
          (calibredb-root-dir-quote)
          (setq calibredb-db-dir (concat (file-name-as-directory calibredb-root-dir) "metadata.db"))
          (calibredb-ref-default-bibliography)
          (when (and
                 (functionp 'sqlite-available-p)
                 (sqlite-available-p)
                 (sqlitep calibredb-db-connection))
            (funcall 'sqlite-close calibredb-db-connection)
            (setq calibredb-db-connection nil))
          (calibredb-search-refresh-or-resume))
      (message "%s does not exists" db))))

(defun calibredb-virtual-library-filter (keyword)
  "Filter the virtual library based on KEYWORD."
  (setq calibredb-virtual-library-name keyword) ; set calibredb-virtual-library-name
  (setq calibredb-tag-filter-p nil)
  (setq calibredb-favorite-filter-p nil)
  (setq calibredb-author-filter-p nil)
  (setq calibredb-date-filter-p nil)
  (setq calibredb-format-filter-p nil)
  (calibredb-search-keyword-filter
   (cdr (assoc keyword calibredb-virtual-library-alist)))
  (calibredb-search-header))

(defun calibredb-virtual-library-list ()
  "List all virtual libraries."
  (interactive)
  (if (eq (length calibredb-virtual-library-alist) 0)
      (message "No virtual libraries. Set `calibredb-virtual-library-alist' with '((name . keywords))." )
    (let ((keyword (completing-read "Switch Virutal Library: " calibredb-virtual-library-alist)))
      (calibredb-virtual-library-filter keyword)
      (message keyword))))

(defun calibredb-virtual-library-next ()
  "Swith to next virtual library."
  (interactive)
  (if (eq (length calibredb-virtual-library-alist) 0)
      (message "No virtual libraries. Set `calibredb-virtual-library-alist' with '((name . keywords))." )
      (let* ((index (setq calibredb-virtual-library-index
                          (if (< calibredb-virtual-library-index (1- (length calibredb-virtual-library-alist)))
                              (1+ calibredb-virtual-library-index) 0)))
             (keyword (car (nth index calibredb-virtual-library-alist))))
        (calibredb-virtual-library-filter keyword)
        (message keyword))))

(defun calibredb-virtual-library-previous ()
  "Swith to previous virtual library."
  (interactive)
  (if (eq (length calibredb-virtual-library-alist) 0)
      (message "No virtual libraries. Set `calibredb-virtual-library-alist' with '((name . keywords))." )
      (let* ((index (setq calibredb-virtual-library-index
                          (if (> calibredb-virtual-library-index 0)
                              (1- calibredb-virtual-library-index)
                            (1- (length calibredb-virtual-library-alist)))))
             (keyword (car (nth index calibredb-virtual-library-alist))))
        (calibredb-virtual-library-filter keyword)
        (message keyword))))

(provide 'calibredb-library)

;;; calibredb-library.el ends here
