;;; calibredb-folder.el --- Support .metadata.calibre folder metadata -*- lexical-binding: t; -*-

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

(defun calibredb-folder-match-decode ()
  "Extract the JSON array from the last line of LOG-STRING and decode its structure."
  (let* ((json-string (with-temp-buffer
                        (insert-file-contents (expand-file-name ".metadata.calibre" calibredb-root-dir))
                        (buffer-string)))
         (entries (json-parse-string json-string :object-type 'alist :array-type 'list :null-object nil)))
    (calibredb-getbooklist
               (let ((no 0))
                 (-mapcat
                  (lambda (entry)
                    (setq no (1+ no))
                    (list `(
                            (:id                 ,(number-to-string (let ((ids (alist-get 'application_id entry)))
                                                                       (if (numberp ids) ids 0))))
                            (:author-sort        ,(or (mapconcat 'identity (alist-get 'authors entry) ",") ""))
                            (:book-dir           "")
                            (:book-cover         nil)
                            (:book-name          "")
                            (:book-format        ,(substring (calibredb-folder-mailcap-mime-to-extn (alist-get 'mime entry)) 1))
                            (:book-pubdate       ,(or (alist-get 'pubdate entry) ""))
                            (:book-title         ,(alist-get 'title entry))
                            (:file-path          ,(concat calibredb-root-dir (alist-get 'lpath entry) ))
                            (:tag                ,(or (mapconcat 'identity (alist-get 'tags entry) ",") ""))
                            (:size               ,(format "%.2f" (/ (or (alist-get 'size entry) 0) 1048576.0)))
                            (:comment            ,(or (alist-get 'comments entry) ""))
                            (:ids                ,(or (mapconcat (lambda(x) (format "%s:%s" (car x) (cdr x)))(alist-get 'identifiers entry) ",") ""))
                            (:publisher          ,(or (alist-get 'publisher entry) ""))
                            (:series             "")
                            (:lang_code          ,(mapconcat 'identity (alist-get 'languages entry) ","))
                            (:last_modified      ,(or (alist-get 'last_modified entry) (alist-get 'pubdate entry) "")))))
                  entries)))))


(defun calibredb-folder-mailcap-mime-to-extn (mime)
  "Return the file extensions EXTN based on the MIME content type."
  (mailcap-parse-mimetypes)
  (if (stringp mime)
      (let ((ext (car (rassoc (downcase mime) mailcap-mime-extensions))))
        (if (string-empty-p ext)
            ".txt"
          ext))
    ".txt"))

(provide 'calibredb-folder)
