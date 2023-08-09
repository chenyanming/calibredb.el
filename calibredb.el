;;; calibredb.el --- Yet another calibre client -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Damon Chan

;; Author: Damon Chan <elecming@gmail.com>
;; URL: https://github.com/chenyanming/calibredb.el
;; Keywords: tools
;; Created: 9 May 2020
;; Version: 2.12.0
;; Package-Requires: ((emacs "25.1") (org "9.3") (transient "0.1.0") (s "1.12.0") (dash "2.17.0") (request "0.3.3") (esxml "0.3.7"))

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

;;;###autoload
(defun calibredb ()
  "Enter calibre Search Buffer."
  (interactive)
  (cond ((null calibredb-db-dir)
         (message "calibredb: calibredb-db-dir is nil! calibredb won't work without it."))
        ((not (file-regular-p calibredb-db-dir))
         (message "calibredb: %s doesn't exist!" calibredb-db-dir))
        (t
         (if (and (functionp 'sqlite-available-p) (sqlite-available-p))
             (unless (sqlitep calibredb-db-connection)
               (calibredb-db-connection)))
         (let ((cand (or calibredb-search-entries
                         (setq calibredb-search-entries (calibredb-candidates)))))
           (unless calibredb-full-entries
             (setq calibredb-full-entries calibredb-search-entries))
           (when (get-buffer (calibredb-search-buffer))
             (kill-buffer (calibredb-search-buffer)))
           ;; Set virtual library name when the first time to launch calibredb
           (if (equal calibredb-search-filter "")
               (setq calibredb-virtual-library-name calibredb-virtual-library-default-name))
           (switch-to-buffer (calibredb-search-buffer))
           (goto-char (point-min))
           (unless (equal cand '(""))   ; not empty library
             (dolist (item cand)
               (let (beg end)
                 (setq beg (point))
                 (insert (car item))
                 (calibredb-detailed-view-insert-image item)
                 (setq end (point))
                 (put-text-property beg end 'calibredb-entry item)
                 (insert "\n")))
             (goto-char (point-min)))
           (calibredb-ref-default-bibliography)
           (unless (eq major-mode 'calibredb-search-mode)
             (calibredb-search-mode))))))

(provide 'calibredb)
;;; calibredb.el ends here
