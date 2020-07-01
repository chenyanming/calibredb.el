;;; calibredb/calibredb-transient.el -*- lexical-binding: t; -*-

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
(require 'calibredb-utils)
(require 'calibredb-library)

;; Transient dispatch

;;;###autoload (autoload 'calibredb-dispatch "calibredb-dispatch" nil t)
(define-transient-command calibredb-dispatch ()
  "Invoke a calibredb command from a list of available commands in *calibredb-search*."
  :man-page "calibredb"
  ["Metadata"
   [("s" "set_metadata"   calibredb-set-metadata-dispatch)
    ;; ("S" "show_metadata"         calibredb-show-metadata)
    ]]
  ["Catalog"
   [("b" "BibTex"   calibredb-catalog-bib-dispatch)]]
  ["File operaion"
   [("a" "Add a file"   calibredb-add)
    ("A" "Add a directory"   calibredb-add-dir)
    ("d" "Remove a file"   calibredb-remove)
    ("e" "Export" calibredb-export-dispatch)
    ("/" "Live Filter" calibredb-search-live-filter)
    ("i" "Edit Annotation" calibredb-edit-annotation)]
   [("o" "Open file"         calibredb-find-file)
    ("O" "Open file other frame"            calibredb-find-file-other-frame)
    ("v" "View details"  calibredb-view)
    ("V" "Open file with default tool"  calibredb-open-file-with-default-tool)
    ("." "Open dired"  calibredb-open-dired)]
   [("m" "Mark" calibredb-mark-and-forward)
    ("u" "Unmark and forward" calibredb-unmark-and-forward)
    ("DEL" "Unmark and backward" calibredb-unmark-and-backward)
    ("f" "Favorite" calibredb-toggle-favorite-at-point)
    ("h" "Highlight" calibredb-toggle-highlight-at-point)
    ("x" "Archive" calibredb-toggle-archive-at-point)]]
  ["Library operaion"
   [("l" "List Libraries"   calibredb-library-list)
   ("S" "Switch library"   calibredb-switch-library)
   ("c" "Clone library"   calibredb-clone)
   ("r" "Refresh Library"   calibredb-search-refresh)]
   [("n" "Next Library"   calibredb-library-next)
    ("p" "Previous Library"   calibredb-library-previous)
    ("t" "Toggle view (Compact/Detail)"   calibredb-toggle-view)]])

(define-transient-command calibredb-entry-dispatch ()
  "Invoke a calibredb command from a list of available commands in *calibredb-entry*."
  :man-page "calibredb"
  ["Metadata"
   [("s" "set_metadata"   calibredb-set-metadata-dispatch)
    ;; ("S" "show_metadata"         calibredb-show-metadata)
    ]]
  ["File operaion"
   [("o" "Open file"         calibredb-find-file)
    ("O" "Open file other frame"            calibredb-find-file-other-frame)
    ("V" "Open file with default tool"  calibredb-open-file-with-default-tool)
    ("." "Open dired"  calibredb-open-dired)]
   [("e" "Export" calibredb-export-dispatch)]])

(define-transient-command calibredb-set-metadata-dispatch ()
  "Dispatch for set-metadata."
  ["Arguments"
   ("-a" "author_sort"  "author_sort:" calibredb-transient-read-metadata-author-sort)
   ("-A" "authors"  "authors:" calibredb-transient-read-metadata-authors)
   ("-c" "comments"  "comments:" calibredb-transient-read-metadata-comments)
   ("-C" "cover" "cover:" calibredb-transient-read-file)
   ("-i" "identifiers"  "identifiers:" read-string)
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
    ("s" "Set metadata"         calibredb-set-metadata--transient)]])

(define-transient-command calibredb-export-dispatch ()
  "Dispatch for export."
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
    ("e" "Export"         calibredb-export)]])

(define-transient-command calibredb-catalog-bib-dispatch ()
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
    ("b" "Update BibTex file"         calibredb-catalog-bib--transient)]])

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

(defun calibredb-transient-read-metadata-tags (prompt _initial-input _history)
  "Read metadata - tags.
Argument PROMPT prompt to show."
  (let ((cand))
    (if (eq major-mode 'calibredb-search-mode)
        (setq cand (cdr (get-text-property (point) 'calibredb-entry nil)))
      (setq cand (get-text-property (point-min) 'calibredb-entry nil)))
    (let ((last-input))
        (let* ((title (calibredb-getattr cand :book-title))
               (id (calibredb-getattr cand :id))
               (init (calibredb-get-init "tags" cand))
               (num (length (calibredb-find-marked-candidates)))
               (input (or last-input (read-string (if (> num 0)
                                                      (concat "Set tags for " (number-to-string num) " items: ")
                                                    (concat prompt id " " title ": ") ) init))))
          ;; set the input as last input, so that all items use the same input
          (setq last-input input)))))

(defun calibredb-transient-read-metadata-comments (prompt _initial-input _history)
  "Read metadata - comments.
Argument PROMPT prompt to show."
  (let ((cand))
    (if (eq major-mode 'calibredb-search-mode)
        (setq cand (cdr (get-text-property (point) 'calibredb-entry nil)))
      (setq cand (get-text-property (point-min) 'calibredb-entry nil)))
    (let ((last-input))
      (let* ((title (calibredb-getattr cand :book-title))
             (id (calibredb-getattr cand :id))
             (init (calibredb-get-init "comments" cand))
             (num (length (calibredb-find-marked-candidates)))
             (input (or last-input (read-string (if (> num 0)
                                                    (concat "Set comments for " (number-to-string num) " items: ")
                                                  (concat prompt id " " title ": ") ) init))))
        ;; set the input as last input, so that all items use the same input
        (setq last-input input)))))

(defun calibredb-transient-read-metadata-author-sort (prompt _initial-input _history)
  "Read metadata - author_sort.
Argument PROMPT prompt to show."
  (let ((cand))
    (if (eq major-mode 'calibredb-search-mode)
        (setq cand (cdr (get-text-property (point) 'calibredb-entry nil)))
      (setq cand (get-text-property (point-min) 'calibredb-entry nil)))
    (let ((last-input))
      (let* ((title (calibredb-getattr cand :book-title))
             (id (calibredb-getattr cand :id))
             (init (calibredb-get-init "author_sort" cand))
             (num (length (calibredb-find-marked-candidates)))
             (input (or last-input (read-string (if (> num 0)
                                                    (concat "Set author_sort for " (number-to-string num) " items: ")
                                                  (concat prompt id " " title ": ") ) init))))
        ;; set the input as last input, so that all items use the same input
        (setq last-input input)))))

(defun calibredb-transient-read-metadata-authors (prompt _initial-input _history)
  "Read metadata - authors.
Argument PROMPT prompt to show."
  (let ((cand))
    (if (eq major-mode 'calibredb-search-mode)
        (setq cand (cdr (get-text-property (point) 'calibredb-entry nil)))
      (setq cand (get-text-property (point-min) 'calibredb-entry nil)))
    (let ((last-input))
      (let* ((title (calibredb-getattr cand :book-title))
             (id (calibredb-getattr cand :id))
             (init (calibredb-get-init "authors" cand))
             (num (length (calibredb-find-marked-candidates)))
             (input (or last-input (read-string (if (> num 0)
                                                    (concat "Set authors for " (number-to-string num) " items: ")
                                                  (concat prompt id " " title ": ") ) init))))
        ;; set the input as last input, so that all items use the same input
        (setq last-input input)))))

(defun calibredb-transient-read-metadata-title (prompt _initial-input _history)
  "Read metadata - title.
Argument PROMPT prompt to show."
  (let ((cand))
    (if (eq major-mode 'calibredb-search-mode)
        (setq cand (cdr (get-text-property (point) 'calibredb-entry nil)))
      (setq cand (get-text-property (point-min) 'calibredb-entry nil)))
    (let ((last-input))
      (let* ((title (calibredb-getattr cand :book-title))
             (id (calibredb-getattr cand :id))
             (init (calibredb-get-init "title" cand))
             (num (length (calibredb-find-marked-candidates)))
             (input (or last-input (read-string (if (> num 0)
                                                    (concat "Set title for " (number-to-string num) " items: ")
                                                  (concat prompt id " " title ": ") ) init))))
        ;; set the input as last input, so that all items use the same input
        (setq last-input input)))))

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
