;;; calibredb-core.el --- Core for calibredb -*- lexical-binding: t; -*-

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

(require 's)
(require 'dash)
(require 'cl-lib)
(require 'cl-macs)
(require 'sql)
(require 'hl-line)
(require 'transient)
(require 'sendmail)
(require 'dired)
(require 'thingatpt)
(require 'pcase)
(require 'mailcap)

(ignore-errors
  (require 'helm)
  (require 'ivy)
  (require 'all-the-icons)
  (require 'icons-in-terminal))

(eval-when-compile (defvar calibredb-detailed-view))
(eval-when-compile (defvar calibredb-full-entries))
(declare-function calibredb-condense-comments "calibredb-search.el")
(declare-function calibredb-attach-icon-for "calibredb-utils.el")
(declare-function calibredb-get-file-path "calibredb-utils.el")


(defgroup calibredb nil
  "calibredb group"
  :group 'calibredb)

(defcustom calibredb-db-dir nil
  "Location of \"metadata.db\" in your calibre library."
  :type 'file
  :group 'calibredb)


(defcustom calibredb-ref-default-bibliography nil
  "BibTex file for current library."
  :type 'file
  :group 'calibredb)

(defvar calibredb-root-dir-quote nil
  "Location of in your calibre library (expanded and quoted).")

(defcustom calibredb-root-dir "~/Documents/Calibre/"
  "Directory containing your calibre library."
  :type 'directory
  :set (lambda (var value)
         (set var value)
         (setq calibredb-db-dir (expand-file-name "metadata.db"
                                                  calibredb-root-dir)))
  :group 'calibredb)

(defcustom calibredb-virtual-library-default-name "Library"
  "The default virtual library name."
  :group 'calibredb
  :type 'string)

(defvar calibredb-virtual-library-name `,calibredb-virtual-library-default-name)

(defcustom calibredb-download-dir nil
  "String with the path to main download directory for ebooks."
  :type 'file
  :group 'calibredb)

(defcustom calibredb-add-delete-original-file nil
  "After adding file, delete original file? (string \"yes\"/\"no\").
yes: Delete without prompt.
no: No deletion without prompt.
nil: Prompt delete or not."
  :type 'string
  :group 'calibredb)

(defcustom calibredb-fetch-covers nil
  "Fetch cover when fetching metadata? (string \"yes\"/\"no\")."
  :type 'string
  :group 'calibredb)

(defcustom calibredb-show-results nil
  "Set Non-nil to show results after fetching metadata."
  :type 'boolean
  :group 'calibredb)

(defcustom calibredb-preferred-format nil
  "Set to your preferred book format."
  :type 'string
  :group 'calibredb)

(defcustom calibredb-library-alist `((,calibredb-root-dir))
  "Alist for all your calibre libraries."
  :type 'alist
  :group 'calibredb)

(defcustom calibredb-virtual-library-alist '()
  "Alist for all your calibre virtual libraries.
1. Left is the virtual library name that shows in the *calibredb-search* header.
2. Right is the filter keywords - `calibredb-search-filter'."
  :type 'alist
  :group 'calibredb)

(defcustom calibredb-program
  (cond
   ((eq system-type 'darwin)
    "/Applications/calibre.app/Contents/MacOS/calibredb")
   (t
    "calibredb"))
  "Executable used to access the calibredb."
  :type 'file
  :group 'calibredb)

(defcustom calibredb-convert-program
  (cond
   ((eq system-type 'darwin)
    "/Applications/calibre.app/Contents/MacOS/ebook-convert")
   (t
    "ebook-convert"))
  "Executable used to convert ebooks."
  :type 'file
  :group 'calibredb)

(defcustom calibredb-device-dir
  (cond ((eq system-type 'gnu/linux)
         "/TODO")
        ((eq system-type 'windows-nt)
         "/TODO")
        ((eq system-type 'darwin)
         "/Volumes/Kindle/documents/")
        (t "/TODO"))
  "Directory of electronic device, e.g. kindle."
  :type 'directory
  :group 'calibredb)

(defcustom calibredb-debug-program
  (cond
   ((eq system-type 'darwin)
    "/Applications/calibre.app/Contents/MacOS/calibre-debug")
   (t
    "calibre-debug"))
  "Executable for calibredb-debug which is used for author_sort algorithm."
  :type 'file
  :group 'calibredb)

(defcustom calibredb-fetch-metadata-program
  (cond
   ((eq system-type 'darwin)
    "/Applications/calibre.app/Contents/MacOS/fetch-ebook-metadata")
   (t
    "fetch-ebook-metadata"))
  "Executable used to fetch ebook metadata."
  :type 'file
  :group 'calibredb)

(defcustom calibredb-fetch-metadata-source-list '("Google" "Amazon.com")
  "Source alist used to fetch ebook metadata."
  :type 'sexp
  :group 'calibredb)

(defcustom calibredb-sql-separator "\3"
  "SQL separator, used in parsing SQL result into list."
  :group 'calibredb
  :type 'string)

(defcustom calibredb-sql-newline "\2"
  "SQL newline, used in parsing SQL result into list."
  :group 'calibredb
  :type 'string)

(defcustom calibredb-id-width 4
  "Width for id.
Set 0 to hide,
Set negative to keep original length."
  :group 'calibredb
  :type 'integer)

(defcustom calibredb-format-width 5
  "Width for file format.
Set 0 to hide,
Set negative to keep original length."
  :group 'calibredb
  :type 'integer)

(defcustom calibredb-tag-width -1
  "Width for tag.
Set 0 to hide,
Set negative to keep original length."
  :group 'calibredb
  :type 'integer)

(defcustom calibredb-ids-width 0
  "Width for ids.
Set 0 to hide,
Set negative to keep original length."
  :group 'calibredb
  :type 'integer)

(defcustom calibredb-title-width 50
  "Width for title.
Set 0 to hide,
Set negative to keep original length."
  :group 'calibredb
  :type 'integer)

(defcustom calibredb-author-width -1
  "Width for author.
Set 0 to hide,
Set negative to keep original length."
  :group 'calibredb
  :type 'integer)

(defcustom calibredb-comment-width 100
  "Width for comment.
Set 0 to hide,
Set negative to keep original length."
  :group 'calibredb
  :type 'integer)

(defcustom calibredb-date-width 11
  "Width for last_modified date.
Set 0 to hide,
Set negative to keep original length."
  :group 'calibredb
  :type 'integer)

(defcustom calibredb-size-show nil
  "Set Non-nil to show size indicator."
  :group 'calibredb
  :type 'boolean)

(define-obsolete-variable-alias 'calibredb-format-icons
  'calibredb-format-all-the-icons "calibredb 2.3.2")

(defcustom calibredb-format-all-the-icons nil
  "Set Non-nil to show file format icons with all-the-icons."
  :group 'calibredb
  :type 'boolean)

(defcustom calibredb-format-icons-in-terminal nil
  "Set Non-nil to show file format icons with icons-in-terminal."
  :group 'calibredb
  :type 'boolean)

(defcustom calibredb-format-character-icons nil
  "Set Non-nil to show file format icons with built-in character icons."
  :group 'calibredb
  :type 'boolean)

(defcustom calibredb-favorite-keyword "favorite"
  "The favorite tag."
  :group 'calibredb
  :type 'string)

(defcustom calibredb-favorite-icon "★"
  "The favorite icon."
  :group 'calibredb
  :type 'string)

(defcustom calibredb-archive-keyword "archive"
  "The archive tag."
  :group 'calibredb
  :type 'string)

(defcustom calibredb-highlight-keyword "highlight"
  "The highlight tag."
  :group 'calibredb
  :type 'string)

(defcustom calibredb-annotation-field "comments"
  "The field to be saved the annotation."
  :group 'calibredb
  :type 'string)

(defcustom calibredb-condense-comments t
  "Condense comments into one line."
  :group 'calibredb
  :type 'boolean)

(defcustom calibredb-entry-render-comments "shr"
  "Render comments in *calibredb-entry* buffer.
1. face: Render with face `calibredb-comment-face'.
2. shr: Render with shr (Simple HTML Render).
3. annotation: Render with `calibredb-edit-annotation-mode'."
  :group 'calibredb
  :type 'string)

(defcustom calibredb-add-duplicate t
  "Add file to calibredb even it is duplicated, when using `calibredb-add'. Set nil to Disable it."
  :type 'boolean
  :group 'calibredb)

(defcustom calibredb-sort-by 'id
  "Sort the results by metadata."
  :type '(choice
          (const id)
          (const title)
          (const format)
          (const author)
          (const date)
          (const pubdate)
          (const tag)
          (const size)
          (const language))
  :group 'calibredb)

(defcustom calibredb-order 'desc
  "Sort the results by order."
  :type '(choice
          (const asc)
          (const desc))
  :group 'calibredb)

(defvar calibredb-query-string-old "
SELECT id, author_sort, path, name, format, pubdate, title, group_concat(DISTINCT tag) AS tag, uncompressed_size, text, last_modified
FROM
  (SELECT sub2.id, sub2.author_sort, sub2.path, sub2.name, sub2.format, sub2.pubdate, sub2.title, sub2.tag, sub2.uncompressed_size, comments.text, sub2.last_modified
  FROM
    (SELECT child.id, child.author_sort, child.path, child.name, child.format, child.pubdate, child.title, child.last_modified, tags.name AS tag, child.uncompressed_size
    FROM
      (SELECT sub.id, sub.author_sort, sub.path, sub.name, sub.format, sub.pubdate, sub.title, sub.last_modified, sub.uncompressed_size, books_tags_link.tag
      FROM
        (SELECT b.id, b.author_sort, b.path, d.name, d.format, b.pubdate, b.title, b.last_modified, d.uncompressed_size
        FROM data AS d
        LEFT OUTER JOIN books AS b
        ON d.book = b.id) AS sub
        LEFT OUTER JOIN books_tags_link
        ON sub.id = books_tags_link.book) AS child
      LEFT OUTER JOIN tags
      ON child.tag = tags.id) as sub2
    LEFT OUTER JOIN comments
    ON sub2.id = comments.book)
GROUP BY id, format"
  "TODO calibre database query statement.")

(defvar calibredb-query-string "
WITH d AS (
    SELECT books.id, books.id as book, format, uncompressed_size, name
    FROM books
    LEFT JOIN data
    ON books.id = data.book
    WHERE data.book IS NULL
    UNION
    SELECT *
    FROM data
), t AS (
    SELECT books_tags_link.book, group_concat(DISTINCT tags.name) AS tag
    FROM books_tags_link
    LEFT JOIN tags
    ON books_tags_link.tag = tags.id
    GROUP BY books_tags_link.book
), p AS (
    SELECT books_publishers_link.book, publishers.name
    FROM books_publishers_link
    LEFT JOIN publishers
    ON books_publishers_link.publisher = publishers.id
), s AS (
    SELECT books_series_link.book, series.name
    FROM books_series_link
    LEFT JOIN series
    ON books_series_link.series = series.id
), l AS (
    SELECT books_languages_link.book, languages.lang_code
    FROM books_languages_link
    LEFT JOIN languages
    ON books_languages_link.lang_code = languages.id
), b AS (
    SELECT *
    FROM books
)
SELECT d.book AS id, b.author_sort, b.path, d.name, group_concat(DISTINCT d.format) as format, b.pubdate, b.title, t.tag, d.uncompressed_size, c.text, group_concat(i.type || ':' || i.val) AS ids, p.name AS publisher, s.name AS series, l.lang_code, b.last_modified
FROM d
LEFT JOIN p
ON d.book = p.book
LEFT JOIN s
ON d.book = s.book
LEFT JOIN t
ON d.book = t.book
LEFT JOIN l
ON d.book = l.book
LEFT JOIN comments AS c
ON d.book = c.book
LEFT JOIN b
ON d.book = b.id
LEFT JOIN identifiers AS i
ON d.book = i.book
GROUP BY d.book"
  "TODO calibre database query statement.")

(defun calibredb-query-search-string (filter)
  "DEPRECATED Return the where part of SQL based on FILTER."
  (format
   "
WHERE id LIKE '%%%s%%'
OR text LIKE '%%%s%%'
OR tag LIKE '%%%s%%'
OR title LIKE '%%%s%%'
OR format LIKE '%%%s%%'
OR author_sort LIKE '%%%s%%'
" filter filter filter filter filter filter))

(defun calibredb-root-dir-quote ()
  "Return expanded and quoted calibredb root dir."
  (setq calibredb-root-dir-quote (shell-quote-argument (expand-file-name calibredb-root-dir))))

(cl-defstruct calibredb-struct
  command option input id library action)

(cl-defstruct calibredb-convert-struct
  input output option)

(defun calibredb-get-action (state)
  "Get the action function from STATE."
  (let ((action (calibredb-struct-action state)))
    (when action
      (if (functionp action)
          action
        (cadr (nth (car action) action))))))

(cl-defun calibredb-command (&key command option input id library action)
  (let* ((command-string (make-calibredb-struct
                          :command command
                          :option option
                          :input input
                          :id id
                          :library library
                          :action action))
         (line (mapconcat #'identity
                          `(,calibredb-program
                            ,(calibredb-struct-command command-string)
                            ,(calibredb-struct-option command-string)
                            ,(calibredb-struct-input command-string)
                            ,(calibredb-struct-id command-string)
                            ,(calibredb-struct-library command-string)) " ")))
    (setq-local inhibit-message t)
    (message "%s" line)
    (message "%s" (shell-command-to-string line))))

(cl-defun calibredb-process (&key command option input id library action)
  (let* ((command-string (make-calibredb-struct
                          :command command
                          :option option
                          :input input
                          :id id
                          :library library
                          :action action))
         (line (mapconcat #'identity
                          `(,calibredb-program
                            ,(calibredb-struct-command command-string)
                            ,(calibredb-struct-option command-string)
                            ,(calibredb-struct-input command-string)
                            ,(calibredb-struct-id command-string)
                            ,(calibredb-struct-library command-string)) " ")))
    (setq-local inhibit-message t)
    (message "%s" line)
    (start-process-shell-command "calibredb" "*calibredb*" line)))

;; TODO
(cl-defun calibredb-convert-process (&key input output option)
  (let* ((command-string (make-calibredb-convert-struct
                          :input input
                          :output output
                          :option option))
         (line (mapconcat #'identity
                          `(,calibredb-convert-program
                            ,(calibredb-convert-struct-input command-string)
                            ,(calibredb-convert-struct-output command-string)
                            ,(calibredb-convert-struct-option command-string)) " ")))
    (setq-local inhibit-message t)
    (message "%s" line)
    (start-process-shell-command "ebook-convert" "*ebook-convert*" line)))

(defun calibredb-chomp (s)
  "Argument S is string."
  (replace-regexp-in-string "[\s\n]+$" "" s))

(defvar calibredb-db-connection nil
  "The EmacSQL database connection.")

(defun calibredb-db-connection ()
  (setq calibredb-db-connection (funcall 'sqlite-open calibredb-db-dir)))

(defun calibredb-query (sql-query)
  "Query calibre database and return the result.
Argument SQL-QUERY is the sqlite sql query string.

The function works by sending SQL-QUERY to `sql-sqlite-program' if emacs < 29 for the
database file defined by `calibredb-db-dir', dump the output to a hidden
buffer called *calibredb-query-output*, then if the sqlite program
terminates successfully, it will return the string of the output
buffer. If the program fails, it will switch to the output buffer and
tell user something’s wrong."
  (if (and (functionp 'sqlite-available-p) (sqlite-available-p))
      (progn
        (unless (sqlitep calibredb-db-connection)
          (calibredb-db-connection))
        (sqlite-execute calibredb-db-connection sql-query) )
   (let ((out-buf " *calibredb-query-output*"))
    (when (get-buffer out-buf)
      (kill-buffer out-buf))
    (if (not (file-exists-p calibredb-db-dir))
        (message "calibredb-query: calibredb-db-dir is nil! calibredb-query won't work without it.")
      (if (zerop (call-process-shell-command
                  (format "%s -separator %s -newline %s -list -nullvalue '' -noheader %s \"%s\""
                          sql-sqlite-program
                          calibredb-sql-separator
                          calibredb-sql-newline
                          (shell-quote-argument (expand-file-name calibredb-db-dir))
                          sql-query)
                  nil (list out-buf t)))
          ;; If this command terminates successfully (return 0)
          ;; Return the output's string
          (with-current-buffer out-buf
            (buffer-string))
        ;; If this command fails return 'error
        (switch-to-buffer out-buf)
        (goto-char (point-min))
        (error "Error: `calibredb-query' can't query \"%s\". switching to its error buffer" (expand-file-name calibredb-db-dir)))))))

(defun calibredb-query-to-alist (query-result)
  "Builds alist out of a full `calibredb-query' query record result.
Argument QUERY-RESULT is the query result generate by sqlite."
  (if query-result
      (let ((spl-query-result (if (and (functionp 'sqlite-available-p) (sqlite-available-p))
                                  query-result
                                (split-string (calibredb-chomp query-result) calibredb-sql-separator))))
        `((:id                     ,(let ((id (nth 0 spl-query-result)))
                                      (if (stringp id)
                                          id
                                        (number-to-string id))))
          (:author-sort            ,(nth 1 spl-query-result))
          (:book-dir               ,(nth 2 spl-query-result))
          (:book-name              ,(nth 3 spl-query-result))
          (:book-format  ,(downcase (or (nth 4 spl-query-result) "")))
          (:book-pubdate           ,(nth 5 spl-query-result))
          (:book-title             ,(nth 6 spl-query-result))
          (:file-path    ,(concat (file-name-as-directory calibredb-root-dir)
                                  (file-name-as-directory (nth 2 spl-query-result))
                                  (nth 3 spl-query-result) "." (downcase (or (nth 4 spl-query-result) ""))))
          (:tag                    ,(or (nth 7 spl-query-result) ""))
          (:size                   ,(format "%.2f" (/ (let ((size (or (nth 8 spl-query-result) "")))
                                                        (if (stringp size)
                                                            (string-to-number size) size)) 1048576.0)))
          (:comment                ,(or (nth 9 spl-query-result) ""))
          (:ids                    ,(or (nth 10 spl-query-result) ""))
          (:publisher              ,(or (nth 11 spl-query-result) ""))
          (:series                 ,(or (nth 12 spl-query-result) ""))
          (:lang_code              ,(or (nth 13 spl-query-result) ""))
          (:last_modified          ,(or (nth 14 spl-query-result) ""))))))

(defun calibredb-getattr (my-alist key)
  "Get the attribute.
Argument MY-ALIST is the alist.
Argument KEY is the key."
  (cadr (assq key (car my-alist))))

(defun calibredb-format-column (string width &optional align)
  "Return STRING truncated or padded to WIDTH following ALIGNment.
ALIGN should be a keyword :left or :right."
  (cond ((< width 0) string)
        ((= width 0) "")
        (t (format (format "%%%s%d.%ds" (if (eq align :left) "-" "") width width)
                   string))))

(defun calibredb-title-face ()
  "Return the title face base on the view."
  (if calibredb-detailed-view
      'calibredb-title-detailed-view-face
      'calibredb-title-face))

(defun calibredb-title-width ()
  "Return the title width base on the view."
  (if calibredb-detailed-view
      -1
    calibredb-title-width))

(defun calibredb-format-width ()
  "Return the format width base on the view."
  (if calibredb-detailed-view
      -1
    calibredb-format-width))

(defun calibredb-tag-width ()
  "Return the tag width base on the view."
  (if calibredb-detailed-view
      -1
    calibredb-tag-width))

(defun calibredb-ids-width ()
  "Return the ids width base on the view."
  (if calibredb-detailed-view
      -1
    calibredb-ids-width))

(defun calibredb-author-width ()
  "Return the author width base on the view."
  (if calibredb-detailed-view
      -1
    calibredb-author-width))

(defun calibredb-comment-width ()
  "Return the comment width base on the view."
  (if calibredb-detailed-view
      -1
    calibredb-comment-width))

(defun calibredb-date-width ()
  "Return the last_modified date width base on the view."
  (if calibredb-detailed-view
      -1
    calibredb-date-width))

(defun calibredb-getbooklist (calibre-item-list)
  "Get book list.
Argument CALIBRE-ITEM-LIST is the calibred item list."
  (let (display-alist)
    (dolist (item calibre-item-list display-alist)
      (setq display-alist
            (cons (list (calibredb-format-item item) item) display-alist)))))

(defun calibredb-candidates ()
  "Generate ebooks candidates alist."
  (let* ((query-result (calibredb-query (concat calibredb-query-string
                                                (pcase calibredb-sort-by
                                                  ('id " ORDER BY id")
                                                  ('title " ORDER BY title")
                                                  ('author " ORDER BY author_sort")
                                                  ('format " ORDER BY format")
                                                  ('date " ORDER BY last_modified")
                                                  ('pubdate " ORDER BY pubdate")
                                                  ('tag " ORDER BY tag")
                                                  ('size " ORDER BY uncompressed_size")
                                                  ('language " ORDER BY lang_code")
                                                  (_ " ORDER BY id"))
                                                (when (eq calibredb-order 'desc)
                                                  " DESC"))))
         (line-list (if (and (functionp 'sqlite-available-p) (sqlite-available-p))
                        query-result
                      (split-string (calibredb-chomp query-result) calibredb-sql-newline) )))
    (cond ((equal "" query-result) '(""))
          ((equal nil query-result) '(""))
          (t (let (res-list h-list f-list a-list)
               (dolist (line line-list)
                 (if (and (functionp 'sqlite-available-p) (sqlite-available-p))
                     (push (calibredb-query-to-alist line) res-list)
                   ;; validate if it is right format
                   (if (string-match-p (concat "^[0-9]\\{1,10\\}" calibredb-sql-separator) line)
                       ;; decode and push to res-list
                       (push (calibredb-query-to-alist line) res-list))))
               ;; filter archive/highlight/favorite items
               (dolist (item res-list)
                 (cond ((string-match-p "archive" (calibredb-getattr (list item) :tag))
                        (setq res-list (remove item res-list))
                        (setq a-list (cons item a-list)))
                       ((string-match-p "favorite" (calibredb-getattr (list item) :tag))
                        (setq res-list (remove item res-list))
                        (setq f-list (cons item f-list)))
                       ((string-match-p "highlight" (calibredb-getattr (list item) :tag))
                        (setq res-list (remove item res-list))
                        (setq h-list (cons item h-list)))))
               ;; merge archive/highlight/favorite/rest items
               (setq res-list (nconc a-list res-list h-list f-list))
               (calibredb-getbooklist res-list))))))

(defun calibredb-candidate(id)
  "Generate one ebook candidate alist.
ARGUMENT ID is the id of the ebook in string."
  (let* ((query-result (calibredb-query (format "SELECT * FROM (%s) WHERE id = %s" calibredb-query-string id)))
         (line-list (if (and (functionp 'sqlite-available-p) (sqlite-available-p))
                        query-result
                      (if query-result (split-string (calibredb-chomp query-result) calibredb-sql-newline)) )))
    (cond ((equal "" query-result) '(""))
          ((equal nil query-result) '(""))
          (t (let (res-list)
               (dolist (line line-list)
                 (if (and (functionp 'sqlite-available-p) (sqlite-available-p))
                     (push (calibredb-query-to-alist line) res-list)
                   ;; validate if it is right format
                   (if (string-match-p (concat "^[0-9]\\{1,10\\}" calibredb-sql-separator) line)
                       ;; decode and push to res-list
                       (push (calibredb-query-to-alist line) res-list)
                     ;; concat the invalid format strings into last line
                     ;; (setf (cadr (assoc :comment (car res-list))) (concat (cadr (assoc :comment (car res-list))) line))
                     )))
               (calibredb-getbooklist res-list)) ))))

(defun calibredb-candidate-query-filter (filter)
  "DEPRECATED Generate ebook candidate alist.
ARGUMENT FILTER is the filter string."
  (let* ((query-result (calibredb-query (format "SELECT * FROM (%s) %s" calibredb-query-string (calibredb-query-search-string filter))))
         (line-list (if query-result (split-string (calibredb-chomp query-result) calibredb-sql-newline))))
    (cond ((equal "" query-result) '(""))
          (t (let (res-list)
               (dolist (line line-list)
                 ;; validate if it is right format
                 (if (string-match-p (concat "^[0-9]\\{1,10\\}" calibredb-sql-separator) line)
                     ;; decode and push to res-list
                     (push (calibredb-query-to-alist line) res-list)
                   ;; concat the invalid format strings into last line
                   ;; (setf (cadr (assoc :comment (car res-list))) (concat (cadr (assoc :comment (car res-list))) line))
                   ))
               (calibredb-getbooklist res-list)) ))))

(defun calibredb-format-item (book-alist)
  "Format the candidate string shown in helm or ivy.
Argument BOOK-ALIST ."
  (let ((id (calibredb-getattr (list book-alist) :id))
        (title (calibredb-getattr (list book-alist) :book-title))
        (format (calibredb-getattr (list book-alist) :book-format))
        (author (calibredb-getattr (list book-alist) :author-sort))
        (tag (calibredb-getattr (list book-alist) :tag))
        (comment (calibredb-getattr (list book-alist) :comment))
        (size (calibredb-getattr (list book-alist) :size))
        (ids (calibredb-getattr (list book-alist) :ids))
        (date (calibredb-getattr (list book-alist) :last_modified))
        (favorite-map (make-sparse-keymap))
        (tag-map (make-sparse-keymap))
        (format-map (make-sparse-keymap))
        (author-map (make-sparse-keymap))
        (date-map (make-sparse-keymap)))
    (define-key favorite-map [mouse-1] 'calibredb-favorite-mouse-1)
    (define-key tag-map [mouse-1] 'calibredb-tag-mouse-1)
    (define-key format-map [mouse-1] 'calibredb-format-mouse-1)
    (define-key author-map [mouse-1] 'calibredb-author-mouse-1)
    (define-key date-map [mouse-1] 'calibredb-date-mouse-1)
    (if calibredb-detailed-view
        (setq title (concat title "\n")))
    (format
     (if calibredb-detailed-view
         (let ((num (cond (calibredb-format-all-the-icons 3)
                          (calibredb-format-icons-in-terminal 3)
                          ((>= calibredb-id-width 0) calibredb-id-width)
                          (t 0 ))))
           (concat
            "%s%s%s"
            (calibredb-format-column (format "%sFormat:" (make-string num ? )) (+ 8 num) :left) "%s\n"
            (calibredb-format-column (format "%sDate:" (make-string num ? )) (+ 8 num) :left) "%s\n"
            (calibredb-format-column (format "%sAuthor:" (make-string num ? ))  (+ 8 num) :left) "%s\n"
            (calibredb-format-column (format "%sTag:" (make-string num ? )) (+ 8 num) :left) "%s\n"
            (calibredb-format-column (format "%sIds:" (make-string num ? )) (+ 8 num) :left) "%s\n"
            (calibredb-format-column (format "%sComment:" (make-string num ? )) (+ 8 num) :left) "%s\n"
            (calibredb-format-column (format "%sSize:" (make-string num ? )) (+ 8 num) :left) "%s"))
       "%s%s%s %s %s %s (%s) %s %s %s")
     (cond (calibredb-format-all-the-icons
            (concat (if (fboundp 'all-the-icons-icon-for-file)
                        (all-the-icons-icon-for-file (calibredb-get-file-path (list book-alist))) "")
                    " "))
           (calibredb-format-icons-in-terminal
            (concat (if (fboundp 'icons-in-terminal-icon-for-file)
                        (icons-in-terminal-icon-for-file (calibredb-get-file-path (list book-alist) ) :v-adjust 0 :height 1) "")
                    " "))
           (calibredb-format-character-icons
            (concat (calibredb-attach-icon-for (calibredb-get-file-path (list book-alist))) " "))
           (t ""))
     (calibredb-format-column (format "%s" (propertize id 'face 'calibredb-id-face 'id id)) calibredb-id-width :left)
     (calibredb-format-column (format "%s%s"
                                      (if (s-contains? calibredb-favorite-keyword tag)
                                          (format "%s " (propertize calibredb-favorite-icon
                                                                    'face 'calibredb-favorite-face
                                                                    'mouse-face 'calibredb-mouse-face
                                                                    'help-echo "Filter the favorite items"
                                                                    'keymap favorite-map)) "")
                                      (cond
                                       ((s-contains? calibredb-archive-keyword tag)
                                        (propertize title 'face 'calibredb-archive-face))
                                       ((s-contains? calibredb-highlight-keyword tag)
                                        (propertize title 'face 'calibredb-highlight-face))
                                       (t
                                        (propertize title 'face (calibredb-title-face))))) (calibredb-title-width) :left)
     (calibredb-format-column (propertize format
                                          'face 'calibredb-format-face
                                          'mouse-face 'calibredb-mouse-face
                                          'help-echo "Filter with this format"
                                          'keymap format-map) (calibredb-format-width) :left)
     (calibredb-format-column (propertize (s-left 10 date) 'face 'calibredb-date-face ; only keep YYYY-MM-DD
                                          'mouse-face 'calibredb-mouse-face
                                          'help-echo "Filter with this date"
                                          'keymap date-map) (calibredb-date-width) :left)
     (calibredb-format-column (mapconcat
                               (lambda (author)
                                 (propertize author
                                             'author author
                                             'face 'calibredb-author-face
                                             'mouse-face 'calibredb-mouse-face
                                             'help-echo (format "Filter with this author: %s" author)
                                             'keymap author-map))
                               (split-string author ",") ",") (calibredb-author-width) :left)
     (calibredb-format-column (mapconcat
                               (lambda (tag)
                                 (propertize tag
                                             'tag tag
                                             'face 'calibredb-tag-face
                                             'mouse-face 'calibredb-mouse-face
                                             'help-echo (format "Filter with this tag: %s" tag)
                                             'keymap tag-map))
                               (split-string tag ",") ",") (calibredb-tag-width) :left)
     (calibredb-format-column (propertize ids 'face 'calibredb-ids-face) (calibredb-ids-width) :left)
     (if (stringp comment)
         (propertize
          (let ((c (if calibredb-condense-comments (calibredb-condense-comments comment) comment))
                (w calibredb-comment-width))
            (cond ((> w 0) (s-truncate w c))
                  ((= w 0) "")
                  (t c)))
          'face 'calibredb-comment-face) "")
     (format "%s%s"
             (if calibredb-size-show
                 (propertize size 'face 'calibredb-size-face) "")
             (if calibredb-size-show
                 (propertize "Mb" 'face 'calibredb-size-face) ""))) ))

(provide 'calibredb-core)

;;; calibredb-core.el ends here
