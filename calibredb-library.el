;;; calibredb-library.el -*- lexical-binding: t; -*-

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

(defvar calibredb-library-index 0)

(declare-function calibredb-ref-default-bibliography "calibredb-utils.el")
(declare-function calibredb-search-refresh-or-resume "calibredb-search.el")

;;;###autoload
(defun calibredb-switch-library ()
  "Swich Calibre Library."
  (interactive)
  (let ((result (read-file-name "Quick switch library: ")))
    (if (file-exists-p (concat (file-name-as-directory result) "metadata.db"))
        (progn
          (setq calibredb-root-dir result)
          (calibredb-root-dir-quote)
          (setq calibredb-db-dir (concat (file-name-as-directory calibredb-root-dir) "metadata.db"))
          (calibredb-ref-default-bibliography)
          (calibredb-search-refresh-or-resume))
      (message "INVALID LIBRARY"))))

;;;###autoload
(defun calibredb-library-list ()
  "Switch library from variable `calibredb-library-alist'.
If under *calibredb-search* buffer, it will auto refresh after
selecting the new item."
  (interactive)
  (let ((result (completing-read "Quick switch library: " calibredb-library-alist)) )
    (if (file-exists-p (concat (file-name-as-directory result) "metadata.db"))
        (progn
          (setq calibredb-root-dir result)
          (calibredb-root-dir-quote)
          (setq calibredb-db-dir (concat (file-name-as-directory calibredb-root-dir) "metadata.db"))
          (calibredb-ref-default-bibliography)
          (calibredb-search-refresh-or-resume))
      (message "INVALID LIBRARY"))))

(defun calibredb-library-previous ()
  "Next library from variable `calibredb-library-alist'.
If under *calibredb-search* buffer, it will auto refresh after
selecting the new item."
  (interactive)
  (let* ((index (setq calibredb-library-index (if (> calibredb-library-index 0)
                                                  (1- calibredb-library-index)
                                                (1- (length calibredb-library-alist)))))
        (result (car (nth index calibredb-library-alist))))
    (if (file-exists-p (concat (file-name-as-directory result) "metadata.db"))
        (progn
          (setq calibredb-root-dir result)
          (calibredb-root-dir-quote)
          (setq calibredb-db-dir (concat (file-name-as-directory calibredb-root-dir) "metadata.db"))
          (calibredb-ref-default-bibliography)
          (calibredb-search-refresh-or-resume))
      (message "INVALID LIBRARY"))))

(defun calibredb-library-next ()
  "Next library from variable `calibredb-library-alist'.
If under *calibredb-search* buffer, it will auto refresh after
selecting the new item."
  (interactive)
  (let* ((index (setq calibredb-library-index (if (< calibredb-library-index (1- (length calibredb-library-alist)))
                                                  (1+ calibredb-library-index) 0)))
        (result (car (nth index calibredb-library-alist))))
    (if (file-exists-p (concat (file-name-as-directory result) "metadata.db"))
        (progn
          (setq calibredb-root-dir result)
          (calibredb-root-dir-quote)
          (setq calibredb-db-dir (concat (file-name-as-directory calibredb-root-dir) "metadata.db"))
          (calibredb-ref-default-bibliography)
          (calibredb-search-refresh-or-resume))
      (message "INVALID LIBRARY"))))

(provide 'calibredb-library)

;;; calibredb-library.el ends here
