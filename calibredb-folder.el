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

(defun calibredb-folder-parse-filter (filter)
  "Parse the elements of a search FILTER into a plist."
  (let ((matches ()))
    (cl-loop for element in (split-string filter) collect
             (when (calibredb-folder-valid-regexp-p element)
               (push element matches)))
    `(,@(if matches
            (list :matches matches)))))

(defun calibredb-folder-valid-regexp-p (regexp)
  "Return t if REGEXP is a valid REGEXP."
  (ignore-errors
    (prog1 t
      (string-match-p regexp ""))))


(defun calibredb-folder-candidates-filter (filter entries)
  "Generate ebook candidate alist.
ARGUMENT FILTER is the filter string.
ARGUMENT ENTRIES is the list of entries."
  (let ((matches (plist-get filter :matches))
        res-list)
    (cond (calibredb-tag-filter-p
           (cl-loop for line in entries do
             (if (eval `(and ,@(cl-loop for regex in matches collect
                                        (unless (equal (calibredb-tag-width) 0) (s-contains? regex (calibredb-getattr (list line) :tag))))))
                 (push line res-list))))
          (calibredb-format-filter-p
           (cl-loop for line in entries do
                    (if (eval `(and ,@(cl-loop for regex in matches collect
                                               (unless (equal (calibredb-format-width) 0) (s-contains? regex (calibredb-getattr (list line) :book-format))))))
                        (push line res-list))))
          (calibredb-author-filter-p
           (cl-loop for line in entries do
                    (if (eval `(and ,@(cl-loop for regex in matches collect
                                               (unless (equal (calibredb-author-width) 0) (s-contains? regex (calibredb-getattr (list line) :author-sort))))))
                        (push line res-list))))
          (calibredb-date-filter-p
           (cl-loop for line in entries do
                    (if (eval `(and ,@(cl-loop for regex in matches collect
                                               (unless (equal (calibredb-date-width) 0) (s-contains? regex (calibredb-getattr (list line) :last_modified))))))
                        (push line res-list))))
          (t (cl-loop for line in entries do
             (if (eval `(and ,@(cl-loop for regex in matches collect
                                        (or
                                         (unless (equal calibredb-id-width 0) (string-match-p regex (calibredb-getattr (list line) :id)))
                                         (unless (equal (calibredb-title-width) 0) (string-match-p regex (calibredb-getattr (list line) :book-title)))
                                         (unless (equal (calibredb-format-width) 0) (string-match-p regex (calibredb-getattr (list line) :book-format)))
                                         (unless (equal (calibredb-tag-width) 0) (string-match-p regex (calibredb-getattr (list line) :tag)))
                                         (unless (equal (calibredb-ids-width) 0) (string-match-p regex (calibredb-getattr (list line) :ids)))
                                         (unless (equal (calibredb-author-width) 0) (string-match-p regex (calibredb-getattr (list line) :author-sort)))
                                         (unless (equal (calibredb-date-width) 0) (string-match-p regex (calibredb-getattr (list line) :last_modified)))
                                         ;; Normally, comments are long, it is necessary to trancate the comments to speed up the searching
                                         ;; except calibredb-comment-width is -1.
                                         (unless (equal (calibredb-comment-width) 0) (string-match-p regex (let ((c (calibredb-getattr (list line) :comment))
                                                                                                                 (w calibredb-comment-width))
                                                                                                             (if (> w 0) (s-truncate w c) c))))))))
                 (push line res-list)))))
    (nreverse res-list)))

(defun calibredb-folder-candidate-filter (id entries)
  "Get the folder candidate by ID in ENTRIES."
  (-first (lambda (entry)
            (string-equal id (calibredb-getattr (list entry) :id)))
          entries))

(defun calibredb-folder-candidate (id)
  "Extract the json array and decode its structure to get the book list, then get the entry by ID."
  (let* ((json-string (with-temp-buffer
                        (insert-file-contents (expand-file-name ".metadata.calibre" calibredb-root-dir))
                        (buffer-string)))
         (entries (json-parse-string json-string :object-type 'alist :array-type 'list :null-object nil)))
    (calibredb-getbooklist
     (list (calibredb-folder-candidate-filter id (calibredb-folder-entries-to-plist entries)) ))))

(defun calibredb-folder-candidates (&optional filter)
  "Extract the json array and decode its structure to get the book list.
ARGUMENT FILTER is the filter string."
  (let* ((json-string (with-temp-buffer
                        (insert-file-contents (expand-file-name ".metadata.calibre" calibredb-root-dir))
                        (buffer-string)))
         (entries (json-parse-string json-string :object-type 'alist :array-type 'list :null-object nil))
         (filter (calibredb-folder-parse-filter calibredb-search-filter)))
    (calibredb-getbooklist
     (calibredb-folder-candidates-filter filter (calibredb-folder-entries-to-plist entries)))))


(defun calibredb-folder-entries-to-plist (entries)
  "Convert folder metadata ENTRIES to plist."
  (let ((unsorted-entries (-mapcat
                           (lambda (entry)
                             ;; use lpath as calibredb internal id, since the id maybe missing in the
                             ;; metadata, use md5 of file path to generate a unique id, it will be the
                             ;; same if the file path does not change.
                             (let* ((lpath (alist-get 'lpath entry))
                                    (lst (nth 5 (file-attributes (expand-file-name lpath calibredb-root-dir)))))
                               (list `((:id                 ,(md5 lpath))
                                       (:author-sort        ,(or (mapconcat 'identity (alist-get 'authors entry) ",") ""))
                                       (:book-dir           "")
                                       (:book-cover         nil)
                                       (:book-name          "")
                                       (:book-format        ,(substring (calibredb-folder-mailcap-mime-to-extn (alist-get 'mime entry)) 1))
                                       (:book-pubdate       ,(or (alist-get 'pubdate entry) ""))
                                       (:book-title         ,(alist-get 'title entry))
                                       (:file-path          ,(expand-file-name lpath calibredb-root-dir))
                                       (:tag                ,(or (mapconcat 'identity (alist-get 'tags entry) ",") ""))
                                       (:size               ,(format "%.2f" (/ (or (alist-get 'size entry) 0) 1048576.0)))
                                       (:comment            ,(or (alist-get 'comments entry) ""))
                                       (:ids                ,(or (mapconcat (lambda(x) (format "%s:%s" (car x) (cdr x)))(alist-get 'identifiers entry) ",") ""))
                                       (:publisher          ,(or (alist-get 'publisher entry) ""))
                                       (:series             "")
                                       (:lang_code          ,(mapconcat 'identity (alist-get 'languages entry) ","))
                                       (:lst ,lst) ;; used for sorting
                                       (:last_modified      ,(let ((lst-md (alist-get 'last_modified entry))
                                                                   (pub-d (alist-get 'pubdate entry)))
                                                               (if lst
                                                                   (format-time-string "%Y-%m-%d %H:%M:%S"
                                                                                       lst)
                                                                 (if (string-equal lst-md "None")
                                                                     (if (string-equal pub-d "None")
                                                                         ""
                                                                       pub-d)
                                                                   lst-md)))))) ))
                           entries)))
    ;; always sort by last modified time
    (sort unsorted-entries
          (lambda (a b)
            (let* ((a-time (calibredb-getattr (list a) :lst))
                   (b-time (calibredb-getattr (list b) :lst)))
              (time-less-p a-time b-time))))))


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
