;;; calibredb-transient.el --- Transient for calibredb -*- lexical-binding: t; -*-

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
    ("s" "set_metadata"   calibredb-set-metadata-dispatch)
    ("g" "filter"   calibredb-filter-dispatch)
    ("o" "sort"   calibredb-sort-dispatch)]
   [("RET" "Open file"         calibredb-find-file)
    ("O" "Open file other frame"            calibredb-find-file-other-frame)
    ("v" "View details"  calibredb-view)
    ("V" "Open file with default tool"  calibredb-open-file-with-default-tool)
    ("," "Quick Look"  calibredb-quick-look)
    ("." "Open dired"  calibredb-open-dired)]
   [("m" "Mark" calibredb-mark-and-forward)
    ("u" "Unmark and forward" calibredb-unmark-and-forward)
    ("DEL" "Unmark and backward" calibredb-unmark-and-backward)
    ("*" "Favorite" calibredb-toggle-favorite-at-point)
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
  ["File operation"
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
   ["Toggle field"
    ("*" "favorite" calibredb-toggle-favorite-at-point)
    ("x" "archive" calibredb-toggle-archive-at-point)
    ("h" "highlight" calibredb-toggle-highlight-at-point)]
   ["Set metadata"
    ("s" "Set metadata With Arguments"         calibredb-set-metadata--transient)
    ("f" "Fetch and set metadata by author and title"  calibredb-fetch-and-set-metadata-by-author-and-title)
    ("i" "Fetch and set metadata by ISBN"  calibredb-fetch-and-set-metadata-by-isbn)
    ("d" "Fetch and set metadata by identifier"  calibredb-fetch-and-set-metadata-by-id)]]
  [("q" "Quit"   transient-quit-one)])

(transient-define-prefix calibredb-filter-dispatch ()
  "Dispatch for filtering the results."
  [["Filter by"
    ("t" "tag"         calibredb-filter-by-tag)
    ("f" "format"         calibredb-filter-by-book-format)
    ("a" "author"         calibredb-filter-by-author-sort)
    ("d" "date"         calibredb-filter-by-last_modified)
    ("l" "library (virtual)"      calibredb-virtual-library-list)
    ("L" "Library"      calibredb-library-list)
    ("r" "reset" calibredb-search-clear-filter)]]
  [("q" "Quit"   transient-quit-one)])

(transient-define-prefix calibredb-sort-dispatch ()
  "Dispatch for sorting the results."
  [["Sort by"
    ("o" "order (toggle)"         calibredb-toggle-order)
    ("i" "id"         calibredb-sort-by-id)
    ("t" "title"      calibredb-sort-by-title)
    ("f" "format"         calibredb-sort-by-format)
    ("a" "author"         calibredb-sort-by-author)
    ("d" "date"      calibredb-sort-by-date)
    ("p" "pubdate"         calibredb-sort-by-pubdate)
    ("T" "Tag"      calibredb-sort-by-tag)
    ("s" "size"      calibredb-sort-by-size)
    ("l" "language"      calibredb-sort-by-language)]]
  [("q" "Quit"   transient-quit-one)])

(transient-define-prefix calibredb-export-dispatch ()
  "Dispatch for export files."
  [["Export"
    ("E" "Export (No conversion)"         calibredb-export-without-conversion-dispatch)
    ("a" "azw3"   calibredb-convert-to-azw3)
    ("d" "docx"   calibredb-convert-to-docx)
    ("e" "epub"   calibredb-convert-to-epub)
    ("f" "fb2"    calibredb-convert-to-fb2)
    ("h" "html"   calibredb-convert-to-html)
    ("H" "htmlz"  calibredb-convert-to-htmlz)
    ("l" "lit"   calibredb-convert-to-lit)
    ("L" "lrf"   calibredb-convert-to-lrf)
    ("m" "mobi"   calibredb-convert-to-mobi)
    ("o" "oeb"   calibredb-convert-to-oeb)
    ("P" "pdb"   calibredb-convert-to-pdb)
    ("p" "pdf"   calibredb-convert-to-pdf)
    ("M" "pml"   calibredb-convert-to-pml)
    ("r" "rb"   calibredb-convert-to-rb)
    ("R" "rtf"   calibredb-convert-to-rtf)
    ("s" "snb"   calibredb-convert-to-snb)
    ("T" "tcr"   calibredb-convert-to-tcr)
    ("t" "txt"   calibredb-convert-to-txt)
    ("z" "txtz"   calibredb-convert-to-txtz)]]
  [("q" "Quit"   transient-quit-one)])

(transient-define-prefix calibredb-export-without-conversion-dispatch ()
  "Dispatch for export files without conversion."
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

(transient-define-prefix calibredb-convert-to-epub-dispatch ()
  "TODO: Dispatch for convert to epub."
  ["Arguments"
   ("-a" "Turn off splitting at page breaks."  "--dont-split-on-page-breaks")
   ("-c" "This option is needed only if you intend to use the EPUB with FBReaderJ."  "--epub-flatten")
   ("-m" "Insert an inline Table of Contents that will appear as part of the main book content."  "--epub-inline-toc")
   ("-o" "Put the inserted inline Table of Contents at the end of the book instead of the start." "--epub-toc-at-end")
   ("-f" "The version of the EPUB file to generate."  "--epub-version")
   ("-p" "Extract the contents of the generated EPUB file to the specified directory. "  "--extract-to")
   ("-r" "Split all HTML files larger than this size (in KB). " "--flow-size")
   ("-s" "This option disables the generation of this cover." "--no-default-epub-cover")
   ("-t" "Do not use SVG for the book cover."  "--no-svg-cover")
   ("-d" "When using an SVG cover, this option will cause the cover to scale to cover the available screen area, but still preserve its aspect ratio (ratio of width to height)" "--preserve-cover-aspect-ratio")
   ("-l" "If specified, the output plugin will try to create output that is as human readable as possible." "--pretty-print")
   ("-A" "Title for any generated in-line table of contents." "--toc-title")]
  [["Export"
    ("e" "Export" calibredb-convert-to-epub)]]
  [("q" "Quit"   transient-quit-one)])

(transient-define-prefix calibredb-convert-to-mobi-dispatch ()
  "TODO: Dispatch for convert to mobi."
  ["Arguments"
   ("-d" "Disable compression of the file contents."  " --dont-compress")
   ("-e" "Extract the contents of the generated MOBI file to the specified folder."  "-extract-to")
   ("-f" "By default calibre generates MOBI files that contain the old MOBI 6 format."  " --mobi-file-type")
   ("-i" "Ignore margins in the input document. " " --mobi-ignore-margins")
   ("-k" "By default calibre converts all images to JPEG format in the output MOBI file."  " --mobi-keep-original-images")
   ("-t" "When adding the Table of Contents to the book, add it at the start of the book instead of the end."  "--mobi-toc-at-start")
   ("-n" "Don't add Table of Contents to the book. Useful if the book has its own table of contents." "--no-inline-toc")
   ("-p" "Tag for MOBI files to be marked as personal documents." "--personal-doc")
   ("-a" "When present, use author sort field as author."  "--prefer-author-sort")
   ("-P" "If specified, the output plugin will try to create output that is as human readable as possible." "--pretty-print")
   ("-s" "Enable sharing of book content via Facebook etc." "--share-not-sync")
   ("-T" "Title for any generated in-line table of contents." "--toc-title")]
  [["Export"
    ("e" "Export" calibredb-convert-to-mobi)]]
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
   [("y" "Copy as calibredb org links"         calibredb-org-link-copy)
    ("f" "Copy as file org links"              calibredb-copy-as-org-link)]]
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
Argument PROMPT prompt to show.
Optional argument TYPE."
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
  "Return the latest used arguments in the `calibredb-export-without-conversion-dispatch' transient."
  (car (alist-get 'calibredb-export-without-conversion-dispatch transient-history)))

(defun calibredb-convert-arguments ()
  "Return the latest used arguments in the `calibredb-convert-to-epub-dispatch' transient."
  (car (alist-get 'calibredb-convert-to-epub-dispatch transient-history)))

(defun calibredb-catalog-bib-arguments ()
  "Return the latest used arguments in the `calibredb-catalog-bib-dispatch' transient."
  (car (alist-get 'calibredb-catalog-bib-dispatch transient-history)))

(provide 'calibredb-transient)

;;; calibredb-transient.el ends here
