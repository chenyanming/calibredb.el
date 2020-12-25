;;; calibredb.el --- Yet another calibre client -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Damon Chan

;; Author: Damon Chan <elecming@gmail.com>
;; URL: https://github.com/chenyanming/calibredb.el
;; Keywords: tools
;; Created: 9 May 2020
;; Version: 2.8.0
;; Package-Requires: ((emacs "25.1") (transient "0.1.0") (s "1.12.0") (dash "2.17.0"))

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

;;;###autoload
(defun calibredb ()
  "Enter calibre Search Buffer."
  (interactive)
  (let ((cand (if calibredb-search-entries
                  calibredb-search-entries
                (progn
                  (setq calibredb-search-entries (calibredb-candidates))
                  (setq calibredb-full-entries calibredb-search-entries)))))
    (cond ((not cand)
           (message "INVALID LIBRARY"))
          (t
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
                 (calibredb-detail-view-insert-image item)
                 (setq end (point))
                 (put-text-property beg end 'calibredb-entry item)
                 (insert "\n")))
             (goto-char (point-min)))
           (calibredb-ref-default-bibliography)
           (unless (eq major-mode 'calibredb-search-mode)
             (calibredb-search-mode))))))

(provide 'calibredb)
;;; calibredb.el ends here
