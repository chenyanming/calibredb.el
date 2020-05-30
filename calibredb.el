;;; calibredb.el --- Yet another calibre client -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Damon Chan

;; Author: Damon Chan <elecming@gmail.com>
;; URL: https://github.com/chenyanming/calibredb.el
;; Keywords: tools
;; Created: 9 May 2020
;; Version: 1.5.1
;; Package-Requires: ((emacs "25.1") (org "9.0") (transient "0.1.0"))

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
;; 1. Ebook dashboard.
;; 2. Manage ebooks, actually not only ebooks!
;; 3. Manage libraries.
;; 4. Another bookmarks solution, by setting the tags and comments.
;; 5. Quick search, filter, make actions on items with ivy and helm.


;;; Code:

(require 'org)
(require 'cl-lib)
(require 'sql)
(require 'hl-line)
(require 'transient)
(ignore-errors
  (require 'helm)
  (require 'ivy)
  (require 'all-the-icons))

(defgroup calibredb nil
  "calibredb group"
  :group 'calibredb)

(defcustom calibredb-db-dir nil
  "Location of \"metadata.db\" in your calibre library."
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

(defcustom calibredb-library-alist `((,calibredb-root-dir))
  "Alist for all your calibre libraries."
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

(defcustom calibredb-id-width 4
  "Width for id.
Set 0 to hide,
Set negative to keep original length."
  :group 'calibredb
  :type 'integer)

(defcustom calibredb-format-width 4
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

(defcustom calibredb-comment-width -1
  "Width for width.
Set 0 to hide,
Set negative to keep original length."
  :group 'calibredb
  :type 'integer)

(defcustom calibredb-size-show nil
  "Set Non-nil to show size indicator."
  :group 'calibredb
  :type 'boolean)

(defcustom calibredb-format-icons nil
  "Set Non-nil to show file format icons."
  :group 'calibredb
  :type 'boolean)

;; faces

(defface calibredb-search-header-highlight-face
  '((t :inherit region :weight bold :underline t))
  "Face for the header at point."
  :group 'calibredb-faces)

(defface calibredb-id-face '((t :inherit font-lock-keyword-face))
  "Face used for id."
  :group 'calibredb-faces)

(defface calibredb-title-face '((t :inherit default))
  "Face used for title."
  :group 'calibredb-faces)

(defface calibredb-author-face '((t :inherit font-lock-variable-name-face))
  "Face used for author."
  :group 'calibredb-faces)

(defface calibredb-format-face '((t :inherit font-lock-string-face))
  "Face used for format."
  :group 'calibredb-faces)

(defface calibredb-comment-face '((t :inherit font-lock-type-face))
  "Face used for comment."
  :group 'calibredb-faces)

(defface calibredb-tag-face '((t :inherit font-lock-warning-face))
  "Face used for tag."
  :group 'calibredb-faces)

(defface calibredb-size-face '((t :inherit font-lock-comment-face))
  "Face used for size."
  :group 'calibredb-faces)

(defvar calibredb-query-string "
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
GROUP BY id"
  "TODO calibre database query statement.")

(defvar calibredb-query-one-entry-string "
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
    ON sub2.id = comments.book) "
  "TODO calibre database query one entry prefix statement.")

(defvar calibredb-helm-map
  (if (boundp 'helm-map)
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map helm-map)
        (define-key map "\M-t" #'calibredb-set-metadata--tags-1)
        (define-key map "\M-c" #'calibredb-set-metadata--comments-1)
        map))
  "Keymap for `calibredb-find-helm'.")

(defvar calibredb-show-entry nil
  "The entry being displayed in this buffer.")

(defvar calibredb-show-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" #'calibredb-entry-dispatch)
    (define-key map "o" #'calibredb-find-file)
    (define-key map "O" #'calibredb-find-file-other-frame)
    (define-key map "v" #'calibredb-open-file-with-default-tool)
    (define-key map "s" #'calibredb-set-metadata-dispatch)
    (define-key map "e" #'calibredb-export-dispatch)
    (define-key map "q" #'calibredb-entry-quit)
    (define-key map "\M-t" #'calibredb-set-metadata--tags)
    (define-key map "\M-a" #'calibredb-set-metadata--author)
    (define-key map "\M-T" #'calibredb-set-metadata--title)
    (define-key map "\M-c" #'calibredb-set-metadata--comments)
    map)
  "Keymap for `calibredb-show-mode'.")

(defvar calibredb-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-3] #'calibredb-search-mouse)
    (define-key map (kbd "<RET>") #'calibredb-search-ret)
    (define-key map "?" #'calibredb-dispatch)
    (define-key map "a" #'calibredb-add)
    (define-key map "A" #'calibredb-add-dir)
    (define-key map "c" #'calibredb-clone)
    (define-key map "d" #'calibredb-remove)
    (define-key map "l" #'calibredb-library-list)
    (define-key map "n" #'calibredb-library-next)
    (define-key map "p" #'calibredb-library-previous)
    (define-key map "s" #'calibredb-set-metadata-dispatch)
    (define-key map "S" #'calibredb-switch-library)
    (define-key map "o" #'calibredb-find-file)
    (define-key map "O" #'calibredb-find-file-other-frame)
    (define-key map "v" #'calibredb-open-file-with-default-tool)
    (define-key map "e" #'calibredb-export-dispatch)
    (define-key map "r" #'calibredb-refresh)
    (define-key map "q" #'calibredb-search-quit)
    (define-key map "\M-t" #'calibredb-set-metadata--tags)
    (define-key map "\M-a" #'calibredb-set-metadata--author)
    (define-key map "\M-T" #'calibredb-set-metadata--title)
    (define-key map "\M-c" #'calibredb-set-metadata--comments)
    map)
  "Keymap for `calibredb-search-mode'.")

(defvar calibredb-search-header-function #'calibredb-search-header
  "Function that returns the string to be used for the Calibredb search header.")

(defcustom calibredb-show-unique-buffers nil
  "When non-nil, every entry buffer gets a unique name.
This allows for displaying multiple show buffers at the same
time."
  :group 'calibredb
  :type 'boolean)

(defcustom calibredb-search-unique-buffers nil
  "When non-nil, every entry buffer gets a unique name.
This allows for displaying multiple serch buffers at the same
time."
  :group 'calibredb
  :type 'boolean)

(defcustom calibredb-show-entry-switch #'switch-to-buffer-other-window
  "Function used to display the calibre entry buffer."
  :group 'calibredb
  :type '(choice (function-item switch-to-buffer-other-window)
                 (function-item switch-to-buffer)
                 (function-item pop-to-buffer)
                 function))

(defcustom calibredb-helm-actions
  (if (fboundp 'helm-make-actions)
      (helm-make-actions
       "Open file"                   'calibredb-find-file
       "Show details"                'calibredb-show-entry
       "Open file other frame"       'calibredb-find-file-other-frame
       "Open file with default tool" 'calibredb-open-file-with-default-tool
       "Open Cover Page"             'calibredb-find-cover
       "set_metadata, tags"          'calibredb-set-metadata--tags
       "set_metadata, comments"      'calibredb-set-metadata--comments
       "set_metadata, --list-fileds" 'calibredb-set-metadata--list-fields
       "show_metadata"               'calibredb-show-metadata
       "Export"                      'calibredb-export
       "remove"                      'calibredb-remove))
  "Default actions for calibredb helm."
  :group 'calibredb
  :type '(alist :key-type string :value-type function))

(if (fboundp 'ivy-set-actions)
    (ivy-set-actions
     'calibredb-ivy-read
     '(("o" (lambda (candidate)
              (calibredb-find-file (cdr candidate))) "Open")
       ("O" (lambda (candidate)
              (calibredb-find-file-other-frame (cdr candidate))) "Find file other frame")
       ("v" (lambda (candidate)
              (calibredb-open-file-with-default-tool (cdr candidate))) "Open with default tool")
       ("s" (lambda (candidate)
              (calibredb-show-entry (cdr candidate))) "Show details")
       ("d" (lambda (candidate)
              (calibredb-remove (cdr candidate))) "Delete ebook")
       ("t" (lambda (candidate)
              (calibredb-set-metadata--tags (cdr candidate))) "Tag ebook")
       ("c" (lambda (candidate)
              (calibredb-set-metadata--comments (cdr candidate)))"Comment ebook")
       ("e" (lambda (candidate)
              (calibredb-export (cdr candidate))) "Export"))))


;; Utility

(defun calibredb-root-dir-quote ()
  "Return expanded and quoted calibredb root dir."
  (setq calibredb-root-dir-quote (shell-quote-argument (expand-file-name calibredb-root-dir))))

(cl-defstruct calibredb-struct
  command option input id library action)

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
    ;; (calibredb-get-action command-string)
    ;; (add-to-list 'display-buffer-alist (cons "\\*Async Shell Command\\*" (cons #'display-buffer-no-window t)))
    ;; (let* ((output-buffer (get-buffer-create "*Async Shell Command*"))
    ;;        (proc (progn
    ;;                (async-shell-command line output-buffer)
    ;;                (get-buffer-process output-buffer))))
    ;;   (if (process-live-p proc)
    ;;       ;; (set-process-sentinel proc #'do-something)
    ;;       nil
    ;;     (message "No process running.")))
    (setq-local inhibit-message t)
    (message line)
    (message (shell-command-to-string line))))

(defun calibredb-chomp (s)
  "Argument S is string."
  (replace-regexp-in-string "[\s\n]+$" "" s))

(defun calibredb-open-with-default-tool (filepath)
  "TODO: consolidate default-opener with dispatcher.
Argument FILEPATH is the file path."
  (if (eq system-type 'windows-nt)
      (start-process "shell-process" "*Messages*"
                     "cmd.exe" "/c" filepath)
    (start-process "shell-process" "*Messages*"
                   (cond ((eq system-type 'gnu/linux)
                          (calibredb-chomp
                           (shell-command-to-string
                            (concat
                             "grep Exec "
                             (if (fboundp 'first)
                                 (first
                                  (delq nil (let ((mime-appname (calibredb-chomp (replace-regexp-in-string
                                                                                  "kde4-" "kde4/"
                                                                                  (shell-command-to-string "xdg-mime query default application/pdf")))))
                                              (mapcar
                                               (lambda (dir) (let ((outdir (concat dir "/" mime-appname))) (if (file-exists-p outdir) outdir)))
                                               '("~/.local/share/applications" "/usr/local/share/applications" "/usr/share/applications"))))) )
                             "|head -1|awk '{print $1}'|cut -d '=' -f 2"))))
                         ((eq system-type 'windows-nt)
                          "start")
                         ((eq system-type 'darwin)
                          "open")
                         (t (message "unknown system!?"))) (expand-file-name filepath))))

(defun calibredb-query (sql-query)
  "Query calibre databse and return the result.
Argument SQL-QUERY is the sqlite sql query string."
  (interactive)
  (if (file-exists-p calibredb-db-dir)
      (shell-command-to-string
       (format "%s -separator \1 %s \"%s\""
               sql-sqlite-program
               (shell-quote-argument (expand-file-name calibredb-db-dir))
               sql-query)) nil))

(defun calibredb-query-to-alist (query-result)
  "Builds alist out of a full `calibredb-query' query record result.
Argument QUERY-RESULT is the query result generate by sqlite."
  (if query-result
      (let ((spl-query-result (split-string (calibredb-chomp query-result) "\1")))
        `((:id                     ,(nth 0 spl-query-result))
          (:author-sort            ,(nth 1 spl-query-result))
          (:book-dir               ,(nth 2 spl-query-result))
          (:book-name              ,(nth 3 spl-query-result))
          (:book-format  ,(downcase (nth 4 spl-query-result)))
          (:book-pubdate           ,(nth 5 spl-query-result))
          (:book-title             ,(nth 6 spl-query-result))
          (:file-path    ,(concat (file-name-as-directory calibredb-root-dir)
                                  (file-name-as-directory (nth 2 spl-query-result))
                                  (nth 3 spl-query-result) "." (downcase (nth 4 spl-query-result))))
          (:tag                    ,(nth 7 spl-query-result))
          (:size                   ,(format "%.2f" (/ (string-to-number (nth 8 spl-query-result) ) 1048576.0) ))
          (:comment                ,(format "%s"
                                            (if (not (nth 9 spl-query-result))
                                                ""
                                              (nth 9 spl-query-result))))))))

(defun calibredb-list ()
  "Generate an org buffer which contain all ebooks' cover image, title and the file link."
  (interactive)
  (let* ((buf-name "*calibredb-list*")
         occur-buf)
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (setq occur-buf (get-buffer-create buf-name))
    (let ((res-list (calibredb-candidates)))
      (with-current-buffer occur-buf
        (erase-buffer)
        (insert "#+STARTUP: inlineimages nofold"))
      (dolist (res res-list)
        (let ((cover (concat (file-name-directory (calibredb-getattr (cdr res) :file-path)) "cover.jpg"))
              (title (calibredb-getattr (cdr res) :book-title))
              (format (calibredb-getattr (cdr res) :book-format))
              (book (calibredb-getattr (cdr res) :file-path)))
          (if (image-type-available-p (intern format))
              (setq cover book))
          (if (file-exists-p cover)
              (with-current-buffer occur-buf
                (insert "\n")
                (insert "#+attr_org: :width 200px\n")
                (insert (concat "[[file:" cover "]]"))
                (insert "\n")
                (org-insert-link nil book title)
                ;; (insert "\n")
                ;; (setq start (point))
                ;; (insert title)
                ;; (calibredb-insert-image cover "")
                ;; (setq end (point))
                (insert "\n"))))))
    (when (buffer-live-p occur-buf)
      (switch-to-buffer-other-window occur-buf)
      (read-only-mode)
      (org-mode)
      (goto-char (point-min)))))

(defun calibredb-getattr (my-alist key)
  "Get the attribute.
Argument MY-ALIST is the alist.
Argument KEY is the key."
  (cadr (assoc key (car my-alist))))

(defun calibredb-insert-image (path alt)
  "TODO: Insert an image for PATH at point, falling back to ALT."
  (cond
   ((not (display-graphic-p))
    (insert alt))
   ;; TODO: add native resizing support once it's official
   ((fboundp 'imagemagick-types)
    (insert-image
     (create-image path 'imagemagick nil
                   :ascent 100
                   :max-width 500
                   :max-height 500)))
   (t
    ;; `create-image' errors out for unsupported image types
    (let ((image (ignore-errors (create-image path nil nil :ascent 100))))
      (if image
          (insert-image image)
        (insert alt))))))

(defun calibredb-find-file (&optional candidate)
  "Open file of the selected item.
Optional argument CANDIDATE is the selected item."
  (interactive)
  (unless candidate
    (if (eq major-mode 'calibredb-search-mode)
        (setq candidate (cdr (get-text-property (point) 'calibredb-entry nil)))
      (setq candidate (get-text-property (point-min) 'calibredb-entry nil))))
  (find-file (calibredb-getattr candidate :file-path)))

(defun calibredb-find-file-other-frame (&optional candidate)
  "Open file in other frame of the selected item.
Optional argument CANDIDATE is the selected item."
  (interactive)
  (unless candidate
    (if (eq major-mode 'calibredb-search-mode)
        (setq candidate (cdr (get-text-property (point) 'calibredb-entry nil)))
      (setq candidate (get-text-property (point-min) 'calibredb-entry nil))))
  (find-file-other-frame (calibredb-getattr candidate :file-path)))

(defun calibredb-open-file-with-default-tool (&optional candidate)
  "Open file with the system default tool.
Optional argument CANDIDATE is the selected item."
  (interactive)
  (unless candidate
    (if (eq major-mode 'calibredb-search-mode)
        (setq candidate (cdr (get-text-property (point) 'calibredb-entry nil)))
      (setq candidate (get-text-property (point-min) 'calibredb-entry nil))))
  (calibredb-open-with-default-tool (calibredb-getattr candidate :file-path)))

;; add

(defun calibredb-add ()
  "Add a file into calibre database."
  (interactive)
  (calibredb-command :command "add"
                     :input (calibredb-complete-file-quote "Add a file to Calibre")
                     :library (format "--library-path %s" (calibredb-root-dir-quote)))
  (if (eq major-mode 'calibredb-search-mode)
      (calibredb)))

(defun calibredb-add-dir (&optional option)
  "Add all files in a directory into calibre database.
By default only files that have extensions of known e-book file
types are added.
Optional argument OPTION is additional options."
  (interactive)
  (calibredb-command :command "add"
                     :input (format "--add %s" (concat (file-name-as-directory (calibredb-complete-file-quote "Add a directory to Calibre")) "*.*"))
                     :option (or option "")
                     :library (format "--library-path %s" (calibredb-root-dir-quote)))
  (if (eq major-mode 'calibredb-search-mode)
      (calibredb)))

(defun calibredb-clone ()
  "Create a clone of the current library.
This creates a new, empty library that has all the same custom
columns, Virtual libraries and other settings as the current
library."
  (interactive)
  (calibredb-command :command "clone"
                     :input (calibredb-complete-file-quote "Clone libary to ")))

(defun calibredb-complete-file-quote (&optional arg &rest rest)
  "Get quoted file name using completion.
Optional argument ARG is the prompt.
Optional argument REST is the rest."
  (let ((file (read-file-name (format "%s: " arg) (pop rest))))
    (shell-quote-argument (expand-file-name file))))

;; remove

(defun calibredb-remove (&optional candidate)
  "Remove the slected item.
Optional argument CANDIDATE is the selected item."
  (interactive)
  (unless candidate
    (if (eq major-mode 'calibredb-search-mode)
        (setq candidate (cdr (get-text-property (point) 'calibredb-entry nil)))
      (setq candidate (get-text-property (point-min) 'calibredb-entry nil))))
  (let ((id (calibredb-getattr candidate :id))
        (title (calibredb-getattr candidate :book-title)))
    (if (yes-or-no-p (concat "Confirm Delete: " id " - " title))
        (calibredb-command :command "remove"
                           :id id
                           :library (format "--library-path %s" (calibredb-root-dir-quote))))
    (cond ((equal major-mode 'calibredb-show-mode)
           (kill-buffer (calibredb-show--buffer-name candidate)) (calibredb-refresh))
          (t (eq major-mode 'calibredb-search-mode)
             (calibredb-refresh)))))

;; set_metadata

(defun calibredb-get-init (name cand)
  "Get the initial value in completing prompt.
Argument NAME is the metadata field name string.
Argument CAND is the candidate."
  (cond ((equal name "tags") (calibredb-getattr cand :tag))
        ((equal name "comments") (calibredb-getattr cand :comment))
        ((equal name "author_sort") (calibredb-getattr cand :author-sort))
        ((equal name "authors") (calibredb-getattr cand :author-sort))
        ((equal name "title") (calibredb-getattr cand :book-title))))

(defun calibredb-set-property (name input)
  "OBSELETE: Set the text property.
Argument NAME is the metadata field name string.
Argument INPUT is the candidate."
  (cond ((equal name "tags") (setf (car (cdr (assoc :tag (car (get-text-property (point-min) 'calibredb-entry nil))))) input))
        ((equal name "comments") (setf (car (cdr (assoc :comment (car (get-text-property (point-min) 'calibredb-entry nil))))) input))
        ((equal name "author_sort") (setf (car (cdr (assoc :author-sort (car (get-text-property (point-min) 'calibredb-entry nil))))) input))
        ((equal name "authors") (setf (car (cdr (assoc :author-sort (car (get-text-property (point-min) 'calibredb-entry nil))))) input))
        ((equal name "title") (setf (car (cdr (assoc :book-title (car (get-text-property (point-min) 'calibredb-entry nil))))) input))))

(defun calibredb-set-metadata (name &rest props)
  "Set metadata on filed NAME on amrked candidates.
Argument PROPS are the additional parameters."
  (let ((candidate (plist-get props :candidate)))
    (unless candidate
      (if (eq major-mode 'calibredb-search-mode)
          (setq candidate (cdr (get-text-property (point) 'calibredb-entry nil)))
        (setq candidate (get-text-property (point-min) 'calibredb-entry nil))))
    (let ((last-input))
      (dolist (cand (cond ((memq this-command '(ivy-dispatching-done)) (list candidate))
                          ((memq this-command '(helm-maybe-exit-minibuffer)) (if (fboundp 'helm-marked-candidates)
                                                                                 (helm-marked-candidates) nil))
                          (t (list candidate))))
        (let* ((title (calibredb-getattr cand :book-title))
               (id (calibredb-getattr cand :id))
               (prompt (plist-get props :prompt))
               (field name)
               (init (calibredb-get-init field cand))
               (input (or last-input (read-string (concat prompt id " " title ": ") init))))
          (calibredb-command :command "set_metadata"
                             :option "--field"
                             :input (format "%s:\"%s\"" field input)
                             :id id
                             :library (format "--library-path \"%s\"" calibredb-root-dir))
          ;; set the input as last input, so that all items use the same input
          (setq last-input input)
          (cond ((equal major-mode 'calibredb-show-mode)
                 ;; set it back, calibredb-show-entry need a correct entry
                 ;; (calibredb-set-property field input)
                 (calibredb-show-refresh))
                ((eq major-mode 'calibredb-search-mode)
                 (calibredb))
                (t nil)))))))

(defun calibredb-set-metadata--tags (&optional candidate)
  "Add tags, divided by comma, on marked CANDIDATEs."
  (interactive)
  (calibredb-set-metadata "tags"
                          :prompt "Add tags for "
                          :candidate candidate))

(defun calibredb-set-metadata--comments (&optional candidate)
  "Add comments on marked CANDIDATEs."
  (interactive)
  (calibredb-set-metadata "comments"
                          :prompt "Add comments for "
                          :candidate candidate))

(defun calibredb-set-metadata--title (&optional candidate)
  "Change title on marked CANDIDATEs."
  (interactive)
  (calibredb-set-metadata "title"
                          :prompt "Change title for "
                          :candidate candidate))

(defun calibredb-set-metadata--author_sort (&optional candidate)
  "Change author on marked CANDIDATEs."
  (interactive)
  (calibredb-set-metadata "author_sort"
                          :prompt "Change author for "
                          :candidate candidate))

(defun calibredb-set-metadata--author (&optional candidate)
  "Change author on marked CANDIDATEs."
  (interactive)
  (calibredb-set-metadata "authors"
                          :prompt "Change author for "
                          :candidate candidate))

(defun calibredb-set-metadata--list-fields (&optional candidate)
  "List the selected CANDIDATE supported fileds."
  (interactive)
  (unless candidate
    (if (eq major-mode 'calibredb-search-mode)
        (setq candidate (cdr (get-text-property (point) 'calibredb-entry nil)))
      (setq candidate (get-text-property (point-min) 'calibredb-entry nil))))
  (let* ((id (calibredb-getattr candidate :id)))
    (message (calibredb-command :command "set_metadata"
                                :option "--list-fields"
                                :id id
                                :library (format "--library-path %s" (calibredb-root-dir-quote))))))

(defun calibredb-set-metadata--transient (&optional candidate)
  "Set metadata for the selected CANDIDATE with transient arguments."
  (interactive)
  (unless candidate
    (if (eq major-mode 'calibredb-search-mode)
        (setq candidate (cdr (get-text-property (point) 'calibredb-entry nil)))
      (setq candidate (get-text-property (point-min) 'calibredb-entry nil))))
  (let ((id (calibredb-getattr candidate :id)))
    (calibredb-command :command "set_metadata"
                       :option (format "--field \"%s\"" (s-join "\" --field \"" (-remove 's-blank? (-flatten (calibredb-set-metadata-arguments)))))
                       :id id
                       :library (format "--library-path \"%s\"" calibredb-root-dir))
    (cond ((equal major-mode 'calibredb-show-mode)
           ;; TODO: set it back, calibredb-show-entry need a correct entry
           ;; (calibredb-set-property field input)
           (calibredb-show-refresh))
          ((eq major-mode 'calibredb-search-mode)
           (calibredb))
          (t nil))))

;; show_metadata

(defun calibredb-show-metadata (&optional candidate)
  "Show selected CANDIDATE metadata."
  (interactive)
  (unless candidate
    (setq candidate (get-text-property (point-min) 'calibredb-entry nil)))
  (let* ((id (calibredb-getattr candidate :id)))
    (calibredb-command :command "show_metadata"
                       :id id
                       :library (format "--library-path %s" (calibredb-root-dir-quote)))))

;; export

(defun calibredb-export (&optional candidate)
  "Export the slected CANDIDATE."
  (interactive)
  (unless candidate
    (if (eq major-mode 'calibredb-search-mode)
        (setq candidate (cdr (get-text-property (point) 'calibredb-entry nil)))
      (setq candidate (get-text-property (point-min) 'calibredb-entry nil))))
  (let ((id (calibredb-getattr candidate :id)))
    (calibredb-command :command "export"
                       :option (s-join " " (-remove 's-blank? (-flatten (calibredb-export-arguments))))
                       :input (format "--to-dir %s" (calibredb-complete-file-quote "Export to (select a path)"))
                       :id id
                       :library (format "--library-path %s" (calibredb-root-dir-quote)))))

(defun calibredb-find-cover (candidate)
  "Open the cover page image of selected CANDIDATE."
  (if (get-buffer "cover.jpg")
      (kill-buffer "cover.jpg"))
  (let* ((path (calibredb-getattr candidate :file-path))
        (cover (concat (file-name-directory path) "cover.jpg")))
    (if (file-exists-p cover)
        (find-file cover)
      ;; (message "No cover")
      )))

(defun calibredb-format-column (string width &optional align)
  "Return STRING truncated or padded to WIDTH following ALIGNment.
ALIGN should be a keyword :left or :right."
  (cond ((< width 0) string)
        ((= width 0) "")
        (t (format (format "%%%s%d.%ds" (if (eq align :left) "-" "") width width)
                   string))))

(defun calibredb-format-item (book-alist)
  "Format the candidate string shown in helm or ivy.
Argument BOOK-ALIST ."
  (format
   "%s%s%s %s %s (%s) %s %s%s"
   (if calibredb-format-icons
       (concat (if (fboundp 'all-the-icons-icon-for-file)
                   (all-the-icons-icon-for-file (calibredb-getattr (list book-alist) :file-path)) "") " ") "")
   (calibredb-format-column (propertize (calibredb-getattr (list book-alist) :id) 'face 'calibredb-id-face) calibredb-id-width :left)
   (calibredb-format-column (propertize (calibredb-getattr (list book-alist) :book-title) 'face 'calibredb-title-face) calibredb-title-width :left)
   (calibredb-format-column (propertize (calibredb-getattr (list book-alist) :book-format) 'face 'calibredb-format-face) calibredb-format-width :left)
   (calibredb-format-column (propertize (calibredb-getattr (list book-alist) :author-sort) 'face 'calibredb-author-face) calibredb-author-width :left)
   (calibredb-format-column (propertize (calibredb-getattr (list book-alist) :tag) 'face 'calibredb-tag-face) calibredb-tag-width :left)
   (if (stringp (calibredb-getattr (list book-alist) :comment))
       (calibredb-format-column (propertize (calibredb-getattr (list book-alist) :comment) 'face 'calibredb-comment-face) calibredb-comment-width :left)
     "")
   (if calibredb-size-show
       (propertize (calibredb-getattr (list book-alist) :size) 'face 'calibredb-size-face) "")
   (if calibredb-size-show
       (propertize "Mb" 'face 'calibredb-size-face) "")))

(defun calibredb-ivy-read ()
  "Ivy read for calibredb."
  (if (fboundp 'ivy-read)
      (let ((cand (calibredb-candidates)))
        (if cand
            (ivy-read "Pick a book: "
                      cand
                      :sort nil         ; actually sort them
                      :caller 'calibredb-ivy-read)
          (message "INVALID LIBRARY")))))

(defun calibredb-getbooklist (calibre-item-list)
  "Get book list.
Argument CALIBRE-ITEM-LIST is the calibred item list."
  (let (display-alist)
    (dolist (item calibre-item-list display-alist)
      (setq display-alist
            (cons (list (calibredb-format-item item) item) display-alist)))))

(defun calibredb-candidates()
  "Generate ebooks candidates alist."
  (let* ((query-result (calibredb-query calibredb-query-string))
         (line-list (if query-result (split-string (calibredb-chomp query-result) "\n"))))
    (cond ((equal "" query-result) '(""))
          (t (let (res-list)
               (dolist (line line-list)
                 ;; validate if it is right format
                 (if (string-match-p "^[0-9]\\{1,10\\}\1" line)
                     ;; decode and push to res-list
                     (push (calibredb-query-to-alist line) res-list)
                   ;; concat the invalid format strings into last line
                   (setf (cadr (assoc :comment (car res-list))) (concat (cadr (assoc :comment (car res-list))) line))))
               (calibredb-getbooklist (nreverse res-list))) ))))

(defun calibredb-candidate(id)
  "Generate one ebook candidate alist.
ARGUMENT ID is the id of the ebook in string."
  (let* ((query-result (calibredb-query (concat calibredb-query-one-entry-string "WHERE id = " id)))
         (line-list (if query-result (split-string (calibredb-chomp query-result) "\n"))))
    (cond ((equal "" query-result) '(""))
          (t (let (res-list)
               (dolist (line line-list)
                 ;; validate if it is right format
                 (if (string-match-p "^[0-9]\\{1,10\\}\1" line)
                     ;; decode and push to res-list
                     (push (calibredb-query-to-alist line) res-list)
                   ;; concat the invalid format strings into last line
                   (setf (cadr (assoc :comment (car res-list))) (concat (cadr (assoc :comment (car res-list))) line))))
               (calibredb-getbooklist (nreverse res-list))) ))))

(defun calibredb-helm-read ()
  "Helm read for calibredb."
  (if (fboundp 'helm)
      (helm :sources (if (fboundp 'helm-build-sync-source)
                          (helm-build-sync-source "calibredb"
                            :header-name (lambda (name)
                                           (concat name " in [" calibredb-root-dir "]"))
                            :candidates 'calibredb-candidates
                            ;; :filtered-candidate-transformer 'helm-findutils-transformer
                            ;; :action-transformer 'helm-transform-file-load-el
                            :persistent-action 'calibredb-find-cover
                            :action 'calibredb-helm-actions
                            ;; :help-message 'helm-generic-file-help-message
                            :keymap calibredb-helm-map
                            :candidate-number-limit 9999
                            ;; :requires-pattern 3
                            ))
            :buffer "*helm calibredb*")))

(defun calibredb-find-helm ()
  "Use helm to list all ebooks details."
  (interactive)
  (calibredb-helm-read))

(defun calibredb-find-counsel ()
  "Use counsel to list all ebooks details."
  (interactive)
  (calibredb-ivy-read))

;; calibredb-mode-map functions

(defun calibredb-set-metadata--tags-1 ()
  "Set metadata tag function used in helm action."
  (interactive)
  (if (fboundp 'with-helm-alive-p)
      (with-helm-alive-p
        (if (fboundp 'helm-exit-and-execute-action)
            (helm-exit-and-execute-action #'calibredb-set-metadata--tags)))))

(defun calibredb-set-metadata--comments-1 ()
  "Set metadata comments function used in helm actions."
  (interactive)
  (if (fboundp 'with-helm-alive-p)
      (with-helm-alive-p
        (if (fboundp 'helm-exit-and-execute-action)
            (helm-exit-and-execute-action #'calibredb-set-metadata--comments)))))

;; Transient dispatch

;;;###autoload (autoload 'calibredb-dispatch "calibredb-dispatch" nil t)
(define-transient-command calibredb-dispatch ()
  "Invoke a calibredb command from a list of available commands in *calibredb-search*."
  :man-page "calibredb"
  ["Metadata"
   [("s" "set_metadata"   calibredb-set-metadata-dispatch)
    ;; ("S" "show_metadata"         calibredb-show-metadata)
    ]]
  ["File operaion"
   [("a" "Add a file"   calibredb-add)
    ("A" "Add a directory"   calibredb-add-dir)
    ("d" "Remove a file"   calibredb-remove)]
   [("o" "Open file"         calibredb-find-file)
    ("O" "Open file other frame"            calibredb-find-file-other-frame)]
   [("v" "Open file with default tool"  calibredb-open-file-with-default-tool)]
   [("e" "Export" calibredb-export-dispatch)]]
  ["Library operaion"
   [("l" "List Libraries"   calibredb-library-list)]
   [("S" "Switch library"   calibredb-switch-library)]
   [("c" "Clone library"   calibredb-clone)]
   [("r" "Refresh Library"   calibredb-refresh)]
   [("n" "Next Library"   calibredb-library-next)]
   [("p" "Previous Library"   calibredb-library-previous)]])

(define-transient-command calibredb-entry-dispatch ()
  "Invoke a calibredb command from a list of available commands in *calibredb-entry*."
  :man-page "calibredb"
  ["Metadata"
   [("s" "set_metadata"   calibredb-set-metadata-dispatch)
    ;; ("S" "show_metadata"         calibredb-show-metadata)
    ]]
  ["File operaion"
   [("o" "Open file"         calibredb-find-file)
    ("O" "Open file other frame"            calibredb-find-file-other-frame)]
   [("v" "Open file with default tool"  calibredb-open-file-with-default-tool)]
   [("e" "Export" calibredb-export-dispatch)]])

(define-transient-command calibredb-set-metadata-dispatch ()
  "Create a new commit or replace an existing commit."
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
    ("a" "author"         calibredb-set-metadata--author)
    ("c" "comments"         calibredb-set-metadata--comments)]
   ["List fields"
    ("l" "list fileds"         calibredb-set-metadata--list-fields)]
   ["Set metadata"
    ("s" "Set metadata"         calibredb-set-metadata--transient)]])

(define-transient-command calibredb-export-dispatch ()
  "TODO: Create a new commit or replace an existing commit."
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
               (input (or last-input (read-string (concat prompt id " " title ": ") init))))
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
             (input (or last-input (read-string (concat prompt id " " title ": ") init))))
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
             (input (or last-input (read-string (concat prompt id " " title ": ") init))))
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
             (input (or last-input (read-string (concat prompt id " " title ": ") init))))
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
             (input (or last-input (read-string (concat prompt id " " title ": ") init))))
        ;; set the input as last input, so that all items use the same input
        (setq last-input input)))))

;; Get

(defun calibredb-set-metadata-arguments ()
  "Return the latest used arguments in the `calibredb-set-metadata-dispatch' transient."
  (car (alist-get 'calibredb-set-metadata-dispatch transient-history)))

(defun calibredb-export-arguments ()
  "Return the latest used arguments in the `calibredb-export-dispatch' transient."
  (car (alist-get 'calibredb-export-dispatch transient-history)))

(define-derived-mode calibredb-show-mode fundamental-mode "calibredb-show"
  "Mode for displaying book entry details.
\\{calibredb-show-mode-map}"
  (setq buffer-read-only t)
  (buffer-disable-undo))

(defun calibredb-show--buffer-name (entry)
  "Return the appropriate buffer name for ENTRY.
The result depends on the value of `calibredb-show-unique-buffers'."
  (if calibredb-show-unique-buffers
      (format "*calibredb-entry-<%s>*"
              (calibredb-getattr entry :book-title))
    "*calibredb-entry*"))

(defun calibredb-search--buffer-name ()
  "Return the appropriate buffer name for ENTRY.
The result depends on the value of `calibredb-search-unique-buffers'."
  (if calibredb-search-unique-buffers
      (format "*calibredb-search-<%s>*" calibredb-root-dir)
    "*calibredb-search*"))

(defun calibredb-show-entry (entry)
  "Display ENTRY in the current buffer."
  (when (get-buffer (calibredb-show--buffer-name entry))
    (kill-buffer (calibredb-show--buffer-name entry)))
  (let* ((buff (get-buffer-create (calibredb-show--buffer-name entry)))
         (file (calibredb-getattr entry :file-path))
         (cover (concat (file-name-directory file) "cover.jpg"))
         (format (calibredb-getattr entry :book-format)))
    (with-current-buffer buff
      ;; (setq start (point))
      ;; (insert title)
      (insert (propertize (calibredb-show-metadata entry) 'calibredb-entry entry))
      ;; (insert book)
      (insert "\n")
      (if (image-type-available-p (intern format))
          (calibredb-insert-image file "")
        (calibredb-insert-image cover ""))
      ;; (setq end (point))
      (calibredb-show-mode)
      (setq calibredb-show-entry entry)
      (goto-char (point-min)))
    (funcall calibredb-show-entry-switch buff)))

(defun calibredb-show-refresh ()
  "Refresh ENTRY in the current buffer."
  (interactive)
  (let* ((entry (get-text-property (point-min) 'calibredb-entry nil)) ; old entry
         (id (calibredb-getattr entry :id))                           ; only get the id
         (query-result (cdr (car (calibredb-candidate id))))          ; get the new entry through SQL query
         (buff (get-buffer-create (calibredb-show--buffer-name query-result)))
         (file (calibredb-getattr query-result :file-path))
         (cover (concat (file-name-directory file) "cover.jpg"))
         (format (calibredb-getattr query-result :book-format)))
    (with-current-buffer buff
      (read-only-mode -1)
      (erase-buffer)
      ;; (setq start (point))
      ;; (insert title)
      (insert (propertize (calibredb-show-metadata query-result) 'calibredb-entry query-result))
      ;; (insert book)
      (insert "\n")
      (if (image-type-available-p (intern format))
          (calibredb-insert-image file "")
        (calibredb-insert-image cover ""))
      (calibredb-show-mode)
      (setq calibredb-show-entry query-result)
      (goto-char (point-min)))))

(defun calibredb-search-buffer ()
  "Create buffer calibredb-search."
  (get-buffer-create "*calibredb-search*"))

(defun calibredb-search-header ()
  "TODO: Return the string to be used as the Calibredb header.
Indicating the library you use."
  (format "%s %s" "Library: " calibredb-root-dir))

(define-derived-mode calibredb-search-mode fundamental-mode "calibredb-search"
  "Major mode for listing calibre entries.
\\{calibredb-search-mode-map}"
  (setq truncate-lines t
        buffer-read-only t
        header-line-format '(:eval (funcall calibredb-search-header-function)))
  (buffer-disable-undo)
  (set (make-local-variable 'hl-line-face) 'calibredb-search-header-highlight-face)
  (hl-line-mode))

(defun calibredb ()
  "Enter calibre Search Buffer."
  (interactive)
  (let ((cand (calibredb-candidates)))
    (cond ((not cand)
           (message "INVALID LIBRARY"))
          (t
           (when (get-buffer (calibredb-search-buffer))
             (kill-buffer (calibredb-search-buffer)))
           (switch-to-buffer (calibredb-search-buffer))
           (goto-char (point-min))
           (unless (equal cand '(""))   ; not empty library
             (dolist (item cand)
               (let (beg end)
                 (setq beg (point))
                 (insert (car item))
                 (setq end (point))
                 (put-text-property beg end 'calibredb-entry item)
                 (insert "\n")))
             (goto-char (point-min))
             (unless (eq major-mode 'calibredb-search-mode)
               (calibredb-search-mode)))))))

(defun calibredb-search-mouse (event)
  "Visit the calibredb-entry click on.
Argument EVENT mouse event."
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (if (not (windowp window))
        (error "No ebook chosen"))
    (calibredb-show-entry (cdr (get-text-property pos 'calibredb-entry nil)))
    (select-window window)
    (goto-char pos)))

(defun calibredb-search-ret ()
  "Visit the calibredb-entry."
  (interactive)
  (calibredb-show-entry (cdr (get-text-property (point) 'calibredb-entry nil))))

(defun calibredb-switch-library ()
  "Swich Calibre Library."
  (interactive)
  (let ((result (read-file-name "Quick switch library: ")) )
    (if (file-exists-p (concat (file-name-as-directory result) "metadata.db"))
        (progn
          (setq calibredb-root-dir result)
          (calibredb-root-dir-quote)
          (setq calibredb-db-dir (concat (file-name-as-directory calibredb-root-dir) "metadata.db"))
          (calibredb))
      (message "INVALID LIBRARY"))))

(defun calibredb-library-list ()
  "Switch library from variable `calibredb-library-alist'.
If under *calibredb-search* buffer, it will auto refresh after
selecting the new item."
  (interactive)
  (let ((result (completing-read "Quick switch library: " calibredb-library-alist)) )
    (if (file-exists-p (concat (file-name-as-directory result) "metadata.db"))
        (progn
         (setq calibredb-root-dir result)
         (calibredb-root-dir-quote)
         (setq calibredb-db-dir (concat (file-name-as-directory calibredb-root-dir) "metadata.db"))
         (calibredb))
      (message "INVALID LIBRARY"))))

(defvar calibredb-library-index 0)

(defun calibredb-library-previous ()
  "Next library from variable `calibredb-library-alist'.
If under *calibredb-search* buffer, it will auto refresh after
selecting the new item."
  (interactive)
  (let* ((index (setq calibredb-library-index (if (> calibredb-library-index 0)
                                                  (1- calibredb-library-index)
                                                (1- (length calibredb-library-alist)))))
        (result (car (nth index calibredb-library-alist))))
    (if (file-exists-p (concat (file-name-as-directory result) "metadata.db"))
        (progn
          (setq calibredb-root-dir result)
          (calibredb-root-dir-quote)
          (setq calibredb-db-dir (concat (file-name-as-directory calibredb-root-dir) "metadata.db"))
          (calibredb))
      (message "INVALID LIBRARY"))))

(defun calibredb-library-next ()
  "Next library from variable `calibredb-library-alist'.
If under *calibredb-search* buffer, it will auto refresh after
selecting the new item."
  (interactive)
  (let* ((index (setq calibredb-library-index (if (< calibredb-library-index (1- (length calibredb-library-alist)))
                                                  (1+ calibredb-library-index) 0)))
        (result (car (nth index calibredb-library-alist))))
    (if (file-exists-p (concat (file-name-as-directory result) "metadata.db"))
        (progn
          (setq calibredb-root-dir result)
          (calibredb-root-dir-quote)
          (setq calibredb-db-dir (concat (file-name-as-directory calibredb-root-dir) "metadata.db"))
          (calibredb))
      (message "INVALID LIBRARY"))))

(defun calibredb-refresh ()
  "Refresh the calibredb."
  (interactive)
  (set-buffer (calibredb-search--buffer-name))
  (if (eq major-mode 'calibredb-search-mode)
      (calibredb))
  (message "calibredb-search refreshed."))

(defun calibredb-search-quit ()
  "Quit the *calibredb-search*."
  (interactive)
  (when (eq major-mode 'calibredb-search-mode)
    (if (get-buffer "*calibredb-search*")
        (kill-buffer "*calibredb-search*"))))

(defun calibredb-entry-quit ()
  "Quit the *calibredb-entry*."
  (interactive)
  (when (eq major-mode 'calibredb-show-mode)
    (if (get-buffer "*calibredb-entry*")
        (kill-buffer "*calibredb-entry*"))))

(provide 'calibredb)
;;; calibredb.el ends here
