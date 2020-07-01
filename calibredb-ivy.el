;;; calibredb-ivy.el -*- lexical-binding: t; -*-

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

(eval-when-compile (defvar calibredb-search-entries))
(eval-when-compile (defvar calibredb-full-entries))

(if (fboundp 'ivy-set-actions)
    (ivy-set-actions
     'calibredb-ivy-read
     '(("o" (lambda (candidate)
              (calibredb-find-file (cdr candidate))) "Open")
       ("O" (lambda (candidate)
              (calibredb-find-file-other-frame (cdr candidate))) "Find file other frame")
       ("v" (lambda (candidate)
              (calibredb-show-entry (cdr candidate))) "View details")
       ("V" (lambda (candidate)
              (calibredb-open-file-with-default-tool (cdr candidate))) "Open with default tool")
       ("d" (lambda (candidate)
              (calibredb-remove (cdr candidate))) "Delete ebook")
       ("t" (lambda (candidate)
              (calibredb-set-metadata--tags (cdr candidate))) "Tag ebook")
       ("c" (lambda (candidate)
              (calibredb-set-metadata--comments (cdr candidate)))"Comment ebook")
       ("e" (lambda (candidate)
              (calibredb-export (cdr candidate))) "Export")
       ("m" (lambda (candidate)
              (mail-add-attachment (calibredb-getattr (cdr candidate) :file-path))) "Mail add attachment")
       ("i" (lambda (candidate)
              (org-insert-link nil (calibredb-getattr (cdr candidate) :file-path) (calibredb-getattr (cdr candidate) :book-title))) "Insert an org link"))))


(defun calibredb-counsel-add-file-action (file)
  "Add marked FILEs."
  (calibredb-command :command "add"
                     :input (shell-quote-argument (expand-file-name file))
                     :library (format "--library-path %s" (calibredb-root-dir-quote))))

(defun calibredb-ivy-read ()
  "Ivy read for calibredb."
  (if (fboundp 'ivy-read)
      (let ((cand (if calibredb-search-entries
                      calibredb-search-entries
                    (progn
                      (setq calibredb-search-entries (calibredb-candidates))
                      (setq calibredb-full-entries calibredb-search-entries)))))
        (if cand
            (ivy-read "Pick a book: "
                      cand
                      :sort nil         ; actually sort them
                      :caller 'calibredb-ivy-read)
          (message "INVALID LIBRARY")))))

(defun calibredb-find-counsel ()
  "Use counsel to list all ebooks details."
  (interactive)
  (calibredb-ivy-read))

(provide 'calibredb-ivy)

;;; calibredb-ivy.el ends here
