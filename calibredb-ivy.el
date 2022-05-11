;;; calibredb-ivy.el --- Ivy/counsel for calibredb -*- lexical-binding: t; -*-

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
(require 'calibredb-search)
(require 'calibredb-faces)
(require 'calibredb-utils)

(eval-when-compile (defvar calibredb-search-entries))
(eval-when-compile (defvar calibredb-full-entries))
(eval-when-compile (defvar counsel-ag-base-command))
(declare-function counsel-ag "counsel")

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
              (mail-add-attachment (calibredb-get-file-path (cdr candidate) t))) "Mail add attachment")
       ("i" (lambda (candidate)
              (unless (featurep 'org)
                (require 'org))
              (if (fboundp 'org-insert-link)
                  (org-insert-link nil (calibredb-get-file-path (cdr candidate) t) (calibredb-getattr (cdr candidate) :book-title)) )) "Insert an org link"))))


(defun calibredb-counsel-add-file-action (arg file)
  "Add marked FILEs.
If prefix ARG is non-nil, keep the files after adding without prompt."
  (interactive "P")
  (let ((output (calibredb-command :command "add"
                                   :input (shell-quote-argument (expand-file-name file))
                                   :library (if calibredb-add-duplicate
                                                (format "--library-path %s -d" (calibredb-root-dir-quote))
                                              (format "--library-path %s" (calibredb-root-dir-quote))))))
    (if (s-contains? "Added book ids" output)
        (cond ((string= calibredb-add-delete-original-file "yes")
               (if arg (message "Adding files succeeded, files were kept.")
                 (calibredb-move-to-trash file)))
              ((string= calibredb-add-delete-original-file "no"))
              (t (unless arg
                   (if (yes-or-no-p
                        (concat "File has been copied to database. Subsequently delete original file? " file))
                       (calibredb-move-to-trash file)))))
      (message "Adding book failed, please add it manually."))))

(defun calibredb-move-to-trash (file)
  "Move the FILE to trash."
  (let ((delete-by-moving-to-trash t))
    (pcase system-type
      ('windows-nt
       (if (fboundp 'system-move-file-to-trash)
           (system-move-file-to-trash file)))
      ('gnu/linux
       (if (fboundp 'move-file-to-trash)
           (move-file-to-trash file)))
      ('darwin
       (let ((trash-directory "~/.Trash"))
         (cond ((featurep 'osx-trash)
                (if (fboundp 'osx-trash-move-file-to-trash)
                    (osx-trash-move-file-to-trash file)))
               ((executable-find "trash")
                (call-process "trash" nil nil nil file))
               (t (move-file-to-trash file))))))))

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

;;;###autoload
(defun calibredb-find-counsel ()
  "Use counsel to list all ebooks details."
  (interactive)
  (calibredb-ivy-read))

(defun calibredb-rga ()
  "Search calibredb with rga, using `counsel-ag'.
1. In `calibredb-search-mode', search in the
`calibredb-root-dir'.
2. In `calibredb-show-mode', search in the corresponding format
under the working directory.
3. In `pdf-view-mode', search in PDF files under the working
directory.
4. In `nov-mode', search in EPUB files under the working
directory."
  (interactive)
  (setq-local counsel-ag-base-command "rga --color never --no-heading --smart-case --line-number --with-filename %s")
  (cond
   ((eq major-mode 'calibredb-search-mode)
    (counsel-ag nil calibredb-root-dir nil "Search Calibredb: "))
   ((eq major-mode 'calibredb-show-mode)
    (let ((format (calibredb-getattr (car (calibredb-find-candidate-at-point)) :book-format))
          (path (calibredb-getattr (car (calibredb-find-candidate-at-point)) :file-path)))
      (cond ((equal format "epub")
             (counsel-ag nil (file-name-directory path) "--rga-adapters=pandoc" "Search EPUB: "))
            ((equal format "pdf")
             (counsel-ag nil (file-name-directory path) "--rga-adapters=poppler" "Search PDF: "))
            (t
             (counsel-ag nil (file-name-directory path) nil (concat "Search " format ": "))))))
   ((eq major-mode 'pdf-view-mode)
    (counsel-ag nil nil "--rga-adapters=poppler" "Search PDF: "))
   ((eq major-mode 'nov-mode)
    (counsel-ag nil nil "--rga-adapters=pandoc" "Search EPUB: "))
   (t
    (message (concat "Calibredb-counsel-ag does not support " (symbol-name major-mode))))))
(provide 'calibredb-ivy)

;;; calibredb-ivy.el ends here
