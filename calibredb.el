;;; calibredb.el --- Yet another calibre client -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Damon Chan

;; Author: Damon Chan <elecming@gmail.com>
;; URL: https://github.com/chenyanming/calibredb.el
;; Keywords: tools
;; Created: 9 May 2020
;; Version: 2.13.0
;; Package-Requires: ((emacs "29.1") (org "9.3") (transient "0.1.0") (s "1.12.0") (dash "2.17.0") (request "0.3.3") (esxml "0.3.7"))
;;
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

;; Yet another [[https://calibre-ebook.com/][calibre]] Emacs client.
;; This package integrates calibre (using *calibredb*) into Emacs.
;; 1. Powerful ebook dashboard.
;; 2. Manage ebooks, actually not only ebooks!
;; 3. Manage Ebook libraries.
;; 4. Another bookmarks solution, by setting the tags and comments.
;; 5. Quick search, filter, make actions on items with ivy and helm.
;; 6. Org-ref support.


;;; Code:

(require 'calibredb-core)
(require 'calibredb-faces)
(require 'calibredb-search)
(require 'calibredb-show)
(require 'calibredb-helm)
(require 'calibredb-ivy)
(require 'calibredb-utils)
(require 'calibredb-annotation)
(require 'calibredb-transient)
(require 'calibredb-library)
(require 'calibredb-org)
(require 'calibredb-opds)
(require 'calibredb-consult)
(require 'calibredb-dired)
(require 'calibredb-folder)

;;;###autoload
(defun calibredb ()
  "Enter calibre Search Buffer."
  (interactive)
  ;; Set virtual library name when the first time to launch calibredb
  (let* ((remaining (cdr (-first (lambda (lib)
                                   (s-contains? (car lib) calibredb-root-dir))
                                 calibredb-library-alist)))
         (library-name (or (assoc-default 'name remaining)
                           calibredb-virtual-library-default-name)))
    (setq calibredb-virtual-library-default-name library-name)
    (setq calibredb-virtual-library-name calibredb-virtual-library-default-name))
  (cond
   ;; opds
   ((s-contains? "http" calibredb-root-dir)
    (switch-to-buffer (calibredb-search-buffer))
    (goto-char (point-min))
    (calibredb-ref-default-bibliography)
    (unless (eq major-mode 'calibredb-search-mode)
      (calibredb-search-mode))
    (calibredb-opds-request-page calibredb-root-dir))
   ;; metadata.db
   ((and (stringp calibredb-db-dir)
         (file-exists-p calibredb-db-dir)
         (s-contains? "metadata.db" calibredb-db-dir))
    (cond ((null calibredb-db-dir)
           (message "calibredb: calibredb-db-dir is nil! calibredb won't work without it."))
          ((not (file-regular-p calibredb-db-dir))
           (message "calibredb: %s doesn't exist!" calibredb-db-dir))
          (t
           (if (and (functionp 'sqlite-available-p) (sqlite-available-p))
               (unless (sqlitep calibredb-db-connection)
                 (calibredb-db-connection)))
           (let ((cand (calibredb-search-keyword-filter calibredb-search-filter)))
             (switch-to-buffer (calibredb-search-buffer))
             (goto-char (point-min))
             (calibredb-ref-default-bibliography)
             (unless (eq major-mode 'calibredb-search-mode)
               (calibredb-search-mode))))))
   ;; .metadata.calibre
   ((and (file-exists-p (expand-file-name ".metadata.calibre" calibredb-root-dir)))
    (switch-to-buffer (calibredb-search-buffer))
    (goto-char (point-min))
    (calibredb-ref-default-bibliography)
    (unless (eq major-mode 'calibredb-search-mode)
      (calibredb-search-mode))
    (calibredb-search-update-buffer :folder (calibredb-folder-candidates)))
   (t
    (message "calibredb: %s is invalid." calibredb-db-dir))))

(provide 'calibredb)
;;; calibredb.el ends here
