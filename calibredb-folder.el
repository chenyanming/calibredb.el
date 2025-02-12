;;; calibredb-folder.el -*- lexical-binding: t; -*-

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

(defcustom calibredb-folder-program (concat (file-name-directory load-file-name) "calibredb-folder.py")
  "The command to list books in the folder that has .metadata.calibre file."
  :type 'string
  :group 'calibredb)

(defun calibredb-folder-match-decode ()
  "Extract the JSON array from the last line of LOG-STRING and decode its structure."
  (let* ((lines (split-string (shell-command-to-string (format "%s %s %s" calibredb-debug-program calibredb-folder-program calibredb-root-dir)) "\n" t))
         (json-string (car (last lines)))
         (entries (json-parse-string json-string :object-type 'alist :array-type 'list :null-object nil)))
    (calibredb-getbooklist
               (let ((no 0))
                 (-mapcat
                  (lambda (entry)
                    (setq no (1+ no))
                    (list `(
                            (:id                 ,(number-to-string no))
                            (:author-sort        ,(or (alist-get 'author_sort entry) ""))
                            (:book-dir           "")
                            (:book-cover         nil)
                            (:book-name          "")
                            (:book-format        ,(substring (calibredb-folder-mailcap-mime-to-extn (alist-get 'mime entry)) 1))
                            (:book-pubdate       ,(or (alist-get 'pubdate entry) ""))
                            (:book-title         ,(alist-get 'title entry))
                            (:file-path          ,(concat calibredb-root-dir (alist-get 'lpath entry) ))
                            (:tag                "")
                            (:size               ,(format "%.2f" (/ (or (alist-get 'size entry) 0) 1048576.0)))
                            (:comment            "")
                            (:ids                ,(number-to-string (let ((ids (alist-get 'application_id entry)))
                                                                      (if (numberp ids) ids 0))))
                            (:publisher          ,(or (alist-get 'book_producer entry) ""))
                            (:series             "")
                            (:lang_code          ,(mapconcat 'identity (alist-get 'languages entry) ","))
                            (:last_modified      ""))))
                  entries)))))


(defun calibredb-folder-mailcap-mime-to-extn (mime)
  "Return the file extensions EXTN based on the MIME content type."
  (mailcap-parse-mimetypes)
  (if (stringp mime)
      (car (rassoc (downcase mime) mailcap-mime-extensions))))

(provide 'calibredb-folder)
