;;; calibredb-transient.el --- Transient for calibredb -*- lexical-binding: t; -*-

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

(declare-function calibredb-get-init "calibredb-utils.el")
(declare-function calibredb-find-marked-candidates "calibredb-utils.el")
(declare-function calibredb-rga "calibredb-ivy.el")
(declare-function calibredb-virtual-library-list "calibredb-library.el")
(declare-function calibredb-virtual-library-next "calibredb-library.el")
(declare-function calibredb-virtual-library-previous "calibredb-library.el")

;; Transient dispatch

(transient-define-prefix calibredb-dispatch ()
  "Invoke a calibredb command from a list of available commands in *calibredb-search*."
  :man-page "calibredb"
  ["File operation"
   [("a" "Add a file"   calibredb-add)
    ("A" "Add a directory"   calibredb-add-dir)
    ("d" "Remove a file"   calibredb-remove)
    ("e" "Export" calibredb-export-dispatch)
    ("s" "set_metadata"   calibredb-set-metadata-dispatch)]
   [("o" "Open file"         calibredb-find-file)
    ("O" "Open file other frame"            calibredb-find-file-other-frame)
    ("v" "View details"  calibredb-view)
    ("V" "Open file with default tool"  calibredb-open-file-with-default-tool)
    ("," "Quick Look"  calibredb-quick-look)
    ("." "Open dired"  calibredb-open-dired)]
   [("m" "Mark" calibredb-mark-and-forward)
    ("u" "Unmark and forward" calibredb-unmark-and-forward)
    ("DEL" "Unmark and backward" calibredb-unmark-and-backward)
    ("f" "Favorite" calibredb-toggle-favorite-at-point)
    ("h" "Highlight" calibredb-toggle-highlight-at-point)
    ("x" "Archive" calibredb-toggle-archive-at-point)]]
  ["Library operation"
   [("l" "List Virtual Libraries"   calibredb-virtual-library-list)
    ("n" "Next Virtual Library"   calibredb-virtual-library-next)
    ("p" "Previous Virtual Library"   calibredb-virtual-library-previous)]
   [("L" "List Libraries"   calibredb-library-list)
    ("N" "Next Library"   calibredb-library-next)
    ("P" "Previous Library"   calibredb-library-previous)
    ("S" "Switch Library"   calibredb-switch-library)]
   [("c" "Clone Library"   calibredb-clone)
    ("r" "Refresh Library"   calibredb-search-refresh-and-clear-filter)
    ("R" "Clear Filter"   calibredb-search-clear-filter)
    ("t" "Toggle view (Compact/Detail)"   calibredb-toggle-view)]]
  ["Other operation"
   [("b" "BibTex"   calibredb-catalog-bib-dispatch)
    ("i" "Edit Annotation" calibredb-edit-annotation)]
   [("'" "Search with rga" calibredb-rga)
    ("/" "Live Filter" calibredb-search-live-filter)]
   [("y" "Yank" calibredb-yank-dispatch)]]
  [("q" "Quit"   transient-quit-one)])

(transient-define-prefix calibredb-entry-dispatch ()
  "Invoke a calibredb command from a list of available commands in *calibredb-entry*."
  :man-page "calibredb"
  ["File operaion"
   [("o" "Open file"         calibredb-find-file)
    ("O" "Open file other frame"            calibredb-find-file-other-frame)
    ("V" "Open file with default tool"  calibredb-open-file-with-default-tool)
    ("." "Quick Look"  calibredb-quick-look)
    ("." "Open dired"  calibredb-open-dired)]
   [("e" "Export" calibredb-export-dispatch)
    ("s" "set_metadata"   calibredb-set-metadata-dispatch)
    ("y" "Yank"   calibredb-yank-dispatch)
    ("'" "Search with rga" calibredb-rga)]]
  [("q" "Quit"   transient-quit-one)])

(transient-define-prefix calibredb-set-metadata-dispatch ()
  "Dispatch for set-metadata."
  ["Arguments"
   ("-a" "author_sort"  "author_sort:" calibredb-transient-read-metadata-author-sort)
   ("-A" "authors"  "authors:" calibredb-transient-read-metadata-authors)
   ("-c" "comments"  "comments:" calibredb-transient-read-metadata-comments)
   ("-C" "cover" "cover:" calibredb-transient-read-file)
   ("-i" "identifiers"  "identifiers:" calibredb-transient-read-metadata-ids)
   ("-l" "languages"  "languages:" read-string)
   ("-p" "pubdate" "pubdate:" transient-read-date)
   ("-P" "publisher" "publisher:" read-string)
   ("-r" "rating"  "rating:" read-string)
   ("-s" "series" "series:" read-string)
   ("-S" "series_index" "series_index:" read-string)
   ("-h" "size" "size:" read-string)
   ("-H" "sort" "sort:" read-string)
   ("-t" "tags" "tags:" calibredb-transient-read-metadata-tags)
   ("-T" "title" "title:" calibredb-transient-read-metadata-title)
   ("-d" "timestamp" "timestamp:" transient-read-date)]
  [["Single Field"
    ("t" "tags"         calibredb-set-metadata--tags)
    ("T" "title"         calibredb-set-metadata--title)
    ("a" "author_sort"         calibredb-set-metadata--author_sort)
    ("c" "comments"         calibredb-set-metadata--comments)]
   ["List fields"
    ("l" "list fileds"         calibredb-set-metadata--list-fields)]
   ["Set metadata"
    ("s" "Set metadata With Arguments"         calibredb-set-metadata--transient)
    ("f" "Fetch and set metadata by author and title"  calibredb-fetch-and-set-metadata-by-author-and-title)
    ("i" "Fetch and set metadata by ISBN"  calibredb-fetch-and-set-metadata-by-isbn)
    ("d" "Fetch and set metadata by identifier"  calibredb-fetch-and-set-metadata-by-id)]]
  [("q" "Quit"   transient-quit-one)])

(transient-define-prefix calibredb-export-dispatch ()
  "Dispatch for export files."
  ["Arguments"
   ("-a" "Do not convert non English characters for the file names"  "--dont-asciiize")
   ("-c" "Do not save cover"  "--dont-save-cover")
   ("-m" "Do not update metadata"  "--dont-update-metadata")
   ("-o" "Do not write opf" "--dont-write-opf")
   ("-f" "Formats to save for each book, comma separated."  "--formats " read-string)
   ("-p" "Progress Reporting"  " --progress")
   ("-r" "Replace whitespace with underscores." "--replace-whitespace")
   ("-s" "Single directory to export all files." "--single-dir")
   ("-t" "Template to control the filename and directory structure."  "--template" read-string)
   ("-d" "Dates format. %d - day, %b - month, %m - month number, %Y - year. Default is: %b, %Y" "--timefmt" read-string)
   ;; ("-m" "Export books to the specified directory. Default is ."  "--to-dir")
   ("-l" "Convert paths to lowercase." "--to-lowercase")
   ("-A" "Export all books in database, ignoring the list of ids" "--all")]
  [["Export"
    ("e" "Export"         calibredb-export)]]
  [("q" "Quit"   transient-quit-one)])

(transient-define-prefix calibredb-catalog-bib-dispatch ()
  "Dispatch for catalog BibTex."
  ["Arguments"
   ("-f" "The fields (comma-separated) to output. Default: all" "--fields " calibredb-transient-read-bib-fields)
   ("-t" "Entry type for BibTeX catalog. Default: book"  "--entry-type " calibredb-transient-read-entry-type)
   ("-s" "Output field to sort on. Default: id" "--sort-by " calibredb-transient-read-bib-sort-by)
   ("-c" "Create a citation for BibTeX entries. Default: True"  " --create-citation " calibredb-transient-read-bib-create-citation)
   ("-p" "Create a file entry if formats is selected for BibTeX entries. Default: True"  "--add-files-path " calibredb-transient-read-bib-add-files-path)
   ("-T" "The template for citation creation from database fields. Default: {authors}{id}" "--citation-template " calibredb-transient-read-bib-citation-template)
   ("-e" "BibTeX file encoding output. Default: utf8"  "--choose-encoding " calibredb-transient-read-choose-encoding)
   ("-E" "BibTeX file encoding flag. Default: strict"  "--choose-encoding-configuration " calibredb-transient-read-choose-encoding-configuration)]
  [["Bibtex"
    ("o" "Find BibTex file"         calibredb-find-bib)
    ("b" "Update BibTex file"         calibredb-catalog-bib--transient)]]
  [("q" "Quit"   transient-quit-one)])

(transient-define-prefix calibredb-yank-dispatch ()
  "Invoke a Yank operation."
  :man-page "calibredb"
  ["Yank operaion"
   [("y" "Copy as org links"         calibredb-copy-as-org-link)]]
  [("q" "Quit"   transient-quit-one)])

(defun calibredb-transient-read-bib-fields (prompt _initial-input _history)
  "TODO: Read a BibTex --fields value.
Argument PROMPT prompt to show."
  (read-string prompt "title,title_sort,author_sort,authors,comments,cover,formats,id,isbn,library_name,ondevice,pubdate,publisher,rating,series_index,series,size,tags,timestamp,uuid,languages,identifiers"))

(defun calibredb-transient-read-bib-sort-by (prompt _initial-input _history)
  "Read a BibTex --sort-by value.
Argument PROMPT prompt to show."
  (completing-read prompt '("author_sort" "id" "rating" "size" "timestamp" "title")))

(defun calibredb-transient-read-bib-create-citation (prompt _initial-input _history)
  "Read a BibTex --create-citation value.
Argument PROMPT prompt to show."
  (completing-read prompt '("True" "False")))

(defun calibredb-transient-read-bib-add-files-path (prompt _initial-input _history)
  "Read a BibTex --add-files-path value.
Argument PROMPT prompt to show."
  (completing-read prompt '("True" "False")))

(defun calibredb-transient-read-bib-citation-template (prompt _initial-input _history)
  "TODO: Read a BibTex --citation-template value.
Argument PROMPT prompt to show."
  (read-string prompt "{author_sort}{authors}{id}{isbn}{pubdate}{title_sort}{publisher}{series_index}{series}{tags}{timestamp}{title}{uuid}"))

(defun calibredb-transient-read-choose-encoding (prompt _initial-input _history)
  "Read a BibTex --choose-encoding value.
Argument PROMPT prompt to show."
  (completing-read prompt '("utf8" "cp1252" "ascii")))

(defun calibredb-transient-read-choose-encoding-configuration (prompt _initial-input _history)
  "Read a BibTex --choose-encoding-configuration value.
Argument PROMPT prompt to show."
  (completing-read prompt '("strict" "replace" "ignore" "backslashreplace")))

(defun calibredb-transient-read-entry-type (prompt _initial-input _history)
  "Read a BibTex --entry-type value.
Argument PROMPT prompt to show."
  (completing-read prompt '("book" "misc" "mixed")))

;; Readers

(defun calibredb-transient-read-file (prompt _initial-input _history)
  "Read a file path.
Argument PROMPT prompt to show."
  (expand-file-name (read-file-name prompt)))

(defun calibredb-transient-read-metadata (prompt _initial-input _history &optional type)
  "Read metadata - title.
Argument PROMPT prompt to show."
  (let ((cand))
    (if (eq major-mode 'calibredb-search-mode)
        (setq cand (cdr (get-text-property (point) 'calibredb-entry nil)))
      (setq cand (get-text-property (point-min) 'calibredb-entry nil)))
    (let ((last-input))
      (let* ((title (calibredb-getattr cand :book-title))
             (id (calibredb-getattr cand :id))
             (init (calibredb-get-init type cand))
             (num (length (calibredb-find-marked-candidates)))
             (input (or last-input (read-string (if (> num 0)
                                                    (concat "Set " type " for " (number-to-string num) " items: ")
                                                  (concat prompt id " " title ": ") ) init))))
        ;; set the input as last input, so that all items use the same input
        (setq last-input input)))))

(defun calibredb-transient-read-metadata-tags (prompt initial-input history)
  (calibredb-transient-read-metadata prompt initial-input history "tags"))

(defun calibredb-transient-read-metadata-ids (prompt initial-input history)
  (calibredb-transient-read-metadata prompt initial-input history "ids"))

(defun calibredb-transient-read-metadata-comments (prompt initial-input history)
  (calibredb-transient-read-metadata prompt initial-input history "comments"))

(defun calibredb-transient-read-metadata-author-sort (prompt initial-input history)
  (calibredb-transient-read-metadata prompt initial-input history "author_sort"))

(defun calibredb-transient-read-metadata-authors (prompt initial-input history)
  (calibredb-transient-read-metadata prompt initial-input history "authors"))

(defun calibredb-transient-read-metadata-title (prompt initial-input history)
  (calibredb-transient-read-metadata prompt initial-input history "title"))

;; Get

(defun calibredb-set-metadata-arguments ()
  "Return the latest used arguments in the `calibredb-set-metadata-dispatch' transient."
  (car (alist-get 'calibredb-set-metadata-dispatch transient-history)))

(defun calibredb-export-arguments ()
  "Return the latest used arguments in the `calibredb-export-dispatch' transient."
  (car (alist-get 'calibredb-export-dispatch transient-history)))

(defun calibredb-catalog-bib-arguments ()
  "Return the latest used arguments in the `calibredb-catalog-bib-dispatch' transient."
  (car (alist-get 'calibredb-catalog-bib-dispatch transient-history)))

(provide 'calibredb-transient)

;;; calibredb-transient.el ends here
