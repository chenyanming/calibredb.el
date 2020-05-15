;;; calibredb.el --- Yet another calibre client -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Damon Chan

;; Author: Damon Chan <elecming@gmail.com>
;; URL: https://github.com/chenyanming/calibredb.el
;; Keywords: tools
;; Created: 9 May 2020
;; Version: 1.4.0
;; Package-Requires: ((emacs "25.1") (org "9.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Yet another [[https://calibre-ebook.com/][calibre]] emacs client.
;; This package integrates calibre (using *calibredb*) into emacs.
;; 1. Manage ebooks, actually not only ebooks!
;; 2. Manage libraries.
;; 2. Another bookmarks solution, by setting the tags and comments.
;; 3. Quick search, filter, make actions on items with ivy and helm.
;; 4. Ebook dashboard.


;;; Code:

(require 'org)
(require 'cl-lib)
(require 'sql)
(ignore-errors
  (require 'helm)
  (require 'ivy)
  (require 'transient)
  (require 'all-the-icons))

(defgroup calibredb nil
  "calibredb group"
  :group 'calibredb)

(defvar calibredb-db-dir nil
  "Location of \"metadata.db\" in your calibre library.")

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
Set < 0 to keep original length."
  :group 'calibredb
  :type 'integer)

(defcustom calibredb-format-width 4
  "Width for file format.
Set 0 to hide,
Set < 0 to keep original length."
  :group 'calibredb
  :type 'integer)

(defcustom calibredb-tag-width -1
  "Width for tag.
Set 0 to hide,
Set < 0 to keep original length."
  :group 'calibredb
  :type 'integer)

(defcustom calibredb-title-width 50
  "Width for title.
Set 0 to hide,
Set < 0 to keep original length."
  :group 'calibredb
  :type 'integer)

(defcustom calibredb-author-width -1
  "Width for author.
Set 0 to hide,
Set < 0 to keep original length."
  :group 'calibredb
  :type 'integer)

(defcustom calibredb-comment-width -1
  "Width for width.
Set 0 to hide,
Set < 0 to keep original length."
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

(defvar calibredb-helm-map
  (if (boundp 'helm-map)
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map helm-map)
        (define-key map (kbd "C-c t") #'calibredb-set-metadata--tags-1)
        (define-key map (kbd "C-c c") #'calibredb-set-metadata--comments-1)
        map))
  "Keymap for `calibredb-find-helm'.")

(defvar calibredb-helm-source
  (if (fboundp 'helm-build-sync-source)
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
  "calibredb helm source.")

(defvar calibredb-show-entry nil
  "The entry being displayed in this buffer.")

(defvar calibredb-show-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cg" #'calibredb-dispatch)
    (define-key map "\C-co" #'calibredb-find-file)
    (define-key map "\C-cO" #'calibredb-find-file-other-frame)
    (define-key map "\C-cv" #'calibredb-open-file-with-default-tool)
    (define-key map "\C-ct" #'calibredb-set-metadata--tags)
    (define-key map "\C-cc" #'calibredb-set-metadata--comments)
    (define-key map "\C-ce" #'calibredb-export)
    map)
  "Keymap for `calibredb-show-mode'.")

(defvar calibredb-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-3] #'calibredb-search-mouse)
    (define-key map (kbd "<RET>") #'calibredb-search-ret)
    (define-key map "\C-ca" #'calibredb-add)
    (define-key map "\C-cc" #'calibredb-clone)
    (define-key map "\C-cd" #'calibredb-remove)
    (define-key map "\C-cl" #'calibredb-library-list)
    (define-key map "\C-cs" #'calibredb-switch-library)
    (define-key map "\C-co" #'calibredb-find-file)
    (define-key map "\C-cO" #'calibredb-find-file-other-frame)
    (define-key map "\C-cv" #'calibredb-open-file-with-default-tool)
    (define-key map "\C-ce" #'calibredb-export)
    map)
  "Keymap for calibredb-search-mode.")

(defvar calibredb-search-header-function #'calibredb-search-header
  "Function that returns the string to be used for the Calibredb search header.")

(defcustom calibredb-show-unique-buffers nil
  "When non-nil, every entry buffer gets a unique name.
This allows for displaying multiple show buffers at the same
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
              (calibredb-find-file (cdr candidate)) ) "Open")
       ("O" (lambda (candidate)
              (calibredb-show-entry (cdr candidate)) ) "Show details")
       ("v" (lambda (candidate)
              (calibredb-open-file-with-default-tool (cdr candidate)) ) "Open with default tool")
       ("V" (lambda (candidate)
              (calibredb-find-file-other-frame (cdr candidate)) ) "Find file other frame")
       ("d" (lambda ()
              (calibredb-remove)) "Delete ebook")
       ("t" (lambda (candidate)
              (calibredb-set-metadata--tags (cdr candidate)) ) "Tag ebook")
       ("c" (lambda (candidate)
              (calibredb-set-metadata--comments (cdr candidate)) )"Comment ebook")
       ("e" (lambda (candidate)
              (calibredb-export (cdr candidate)) )"Export")
       ("q"
        (lambda ()
          (message "cancelled")) "(or anything else) to cancel"))))

;; faces

(defface calibredb-search-header-highlight-face
  '((t :inherit region :weight bold :underline t))
  "Face for the header at point."
  :group 'calibredb-faces)

;; Utility

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
         (line (mapconcat 'identity
                          `(,calibredb-program
                            ,(calibredb-struct-command command-string)
                            ,(calibredb-struct-option command-string)
                            ,(calibredb-struct-input command-string)
                            ,(calibredb-struct-id command-string)
                            ,(calibredb-struct-library command-string)) " ")))
    ;; (message line)
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
    (shell-command-to-string line)))

(defun calibredb-chomp (s)
  (replace-regexp-in-string "[\s\n]+$" "" s))

(defun calibredb-open-with-default-tool (filepath)
  ;; TODO: consolidate default-opener with dispatcher
  (if (eq system-type 'windows-nt)
      (start-process "shell-process" "*Messages*"
                     "cmd.exe" "/c" filepath)
    (start-process "shell-process" "*Messages*"
                   (cond ((eq system-type 'gnu/linux)
                          (calibredb-chomp
                           (shell-command-to-string
                            (concat
                             "grep Exec "
                             (first
                              (delq nil (let ((mime-appname (calibredb-chomp (replace-regexp-in-string
                                                                              "kde4-" "kde4/"
                                                                              (shell-command-to-string "xdg-mime query default application/pdf")))))
                                          (mapcar
                                           #'(lambda (dir) (let ((outdir (concat dir "/" mime-appname))) (if (file-exists-p outdir) outdir)))
                                           '("~/.local/share/applications" "/usr/local/share/applications" "/usr/share/applications")))))
                             "|head -1|awk '{print $1}'|cut -d '=' -f 2"))))
                         ((eq system-type 'windows-nt)
                          "start")
                         ((eq system-type 'darwin)
                          "open")
                         (t (message "unknown system!?"))) filepath)))

(defun calibredb-query (sql-query)
  "Query calibre databse and return the result."
  (interactive)
  (shell-command-to-string
   (format "%s -separator \1 %s \"%s\""
           sql-sqlite-program
           (shell-quote-argument calibredb-db-dir)
           sql-query)))

(defun calibredb-query-to-alist (query-result)
  "Builds alist out of a full calibredb-query query record result."
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
  "Generate an org buffer which contains all ebooks' cover image, title and the file link."
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
              (book (calibredb-getattr (cdr res) :file-path)))
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
  (cadr (assoc key (car my-alist))))

(defun calibredb-insert-image (path alt)
  "TODO: Insert an image for PATH at point, falling back to ALT."
  (cond
   ((not (display-graphic-p))
    (insert alt))
   ;; TODO: add native resizing support once it's official
   ((fboundp 'imagemagick-types)
    (let ((edges (window-inside-pixel-edges
                  (get-buffer-window (current-buffer)))))
      (insert-image
       (create-image path 'imagemagick nil
                     :ascent 100
                     :max-width 500
                     :max-height 500))))
   (t
    ;; `create-image' errors out for unsupported image types
    (let ((image (ignore-errors (create-image path nil nil :ascent 100))))
      (if image
          (insert-image image)
        (insert alt))))))

(defun calibredb-find-file (&optional candidate)
  (interactive)
  (unless candidate
    (if (eq major-mode 'calibredb-search-mode)
        (setq candidate (cdr (get-text-property (point) 'calibredb-entry nil)))
      (setq candidate (get-text-property (point-min) 'calibredb-entry nil))))
  (find-file (calibredb-getattr candidate :file-path)))

(defun calibredb-find-file-other-frame (&optional candidate)
  (interactive)
  (unless candidate
    (if (eq major-mode 'calibredb-search-mode)
        (setq candidate (cdr (get-text-property (point) 'calibredb-entry nil)))
      (setq candidate (get-text-property (point-min) 'calibredb-entry nil))))
  (find-file-other-frame (calibredb-getattr candidate :file-path)))

(defun calibredb-open-file-with-default-tool (&optional candidate)
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
                     :input (calibredb-complete-file "Add file to Calibre")
                     :library (format "--library-path \"%s\"" calibredb-root-dir))
  (if (eq major-mode 'calibredb-search-mode)
      (calibredb)))

(defun calibredb-clone ()
  "Create a clone of the current library.
This creates a new, empty library that has all the same custom
columns, Virtual libraries and other settings as the current
library."
  (interactive)
  (calibredb-command :command "clone"
                     :input (calibredb-complete-file "Clone libary to ")))

(defun calibredb-complete-file (&optional arg &rest rest)
  "Get file name using completion."
  (let ((file (read-file-name (format "%s: " arg) (pop rest))))
    (expand-file-name file)))

;; remove

(defun calibredb-remove (&optional candidate)
  (interactive)
  (unless candidate
    (setq candidate (cdr (get-text-property (point) 'calibredb-entry nil))))
  (let ((id (calibredb-getattr candidate :id))
        (title (calibredb-getattr candidate :book-title)))
    (if (yes-or-no-p (concat "Confirm Delete: " id " - " title))
        (calibredb-command :command "remove"
                           :id id
                           :library (format "--library-path \"%s\"" calibredb-root-dir)))
    (if (eq major-mode 'calibredb-search-mode)
        (calibredb))))

;; set_metadata

(defun calibredb-set-metadata--tags (&optional candidate)
  "Add tags, divided by comma, on marked candidates."
  (interactive)
  (unless candidate
    (setq candidate (get-text-property (point-min) 'calibredb-entry nil)))
  (let ((last-input))
    (dolist (cand (cond ((memq this-command '(ivy-dispatching-done)) (list candidate))
                        ((memq this-command '(helm-maybe-exit-minibuffer)) (helm-marked-candidates))
                        (t (list candidate))))
      (let* ((title (calibredb-getattr cand :book-title))
             (tag (calibredb-getattr cand :tag))
             (id (calibredb-getattr cand :id))
             (input (or last-input (read-string (concat "Add tags for " title ": ") tag))))
        (calibredb-command :command "set_metadata"
                           :option "--field"
                           :input (format "tags:\"%s\"" input)
                           :id id
                           :library (format "--library-path \"%s\"" calibredb-root-dir))
        (setq last-input input)
        (when (equal major-mode 'calibredb-show-mode)
          ;; set the comments back, calibredb-show-entry need a correct entry
          (setf (car (cdr (assoc :tag (car (get-text-property (point-min) 'calibredb-entry nil))))) input)
          (calibredb-show-refresh))))))

(defun calibredb-set-metadata--comments (&optional candidate)
  "Add comments on one candidate."
  (interactive)
  (unless candidate
    (setq candidate (get-text-property (point-min) 'calibredb-entry nil)))
  (let* ((title (calibredb-getattr candidate :book-title))
         (comment (calibredb-getattr candidate :comment))
         (id (calibredb-getattr candidate :id))
         (input (read-string (concat "Add comments for " title ": ") comment)))
    (calibredb-command :command "set_metadata"
                       :option "--field"
                       :input (format "comments:\"%s\"" input)
                       :id id
                       :library (format "--library-path \"%s\"" calibredb-root-dir))
    (when (equal major-mode 'calibredb-show-mode)
      ;; set the comments back, calibredb-show-entry need a correct entry
      (setf (car (cdr (assoc :comment (car (get-text-property (point-min) 'calibredb-entry nil))))) input)
      (calibredb-show-refresh))))

(defun calibredb-set-metadata--list-fields (&optional candidate)
  "List the selected candidate supported fileds."
  (interactive)
  (unless candidate
    (setq candidate (get-text-property (point-min) 'calibredb-entry nil)))
  (let* ((id (calibredb-getattr candidate :id)))
    (message (calibredb-command :command "set_metadata"
                                :option "--list-fields"
                                :id id
                                :library (format "--library-path \"%s\"" calibredb-root-dir)))))

;; show_metadata

(defun calibredb-show-metadata (&optional candidate)
  "Show selected candidate metadata."
  (interactive)
  (unless candidate
    (setq candidate (get-text-property (point-min) 'calibredb-entry nil)))
  (let* ((id (calibredb-getattr candidate :id)))
    (calibredb-command :command "show_metadata"
                       :id id
                       :library (format "--library-path \"%s\"" calibredb-root-dir))))

;; export

(defun calibredb-export (&optional candidate)
  (interactive)
  "TODO: Export the selected candidate."
  (unless candidate
    (if (eq major-mode 'calibredb-search-mode)
        (setq candidate (cdr (get-text-property (point) 'calibredb-entry nil)))
      (setq candidate (get-text-property (point-min) 'calibredb-entry nil))))
  (let ((id (calibredb-getattr candidate :id)))
    (calibredb-command :command "export"
                       :input (format "--to-dir \"%s\"" (calibredb-complete-file "Export to (select a path)"))
                       :id id
                       :library (format "--library-path \"%s\"" calibredb-root-dir))))

(defun calibredb-find-cover (candidate)
  "Open the cover page image of selected candidate."
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
Align should be a keyword :left or :right."
  (cond ((< width 0) string)
        ((= width 0) "")
        (t (format (format "%%%s%d.%ds" (if (eq align :left) "-" "") width width)
                   string))))

(defun calibredb-format-item (book-alist)
  "Format the candidate string shown in helm or ivy."
  (format
   "%s%s%s %s %s (%s) %s %s%s"
   (if calibredb-format-icons
       (concat (if (fboundp 'all-the-icons-icon-for-url)
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
  (if (fboundp 'ivy-read)
      (ivy-read "Pick a book: "
                (calibredb-candidates)
                :sort nil               ; actually sort them
                :caller 'calibredb-ivy-read)))


(defun calibredb-getbooklist (calibre-item-list)
  (let (display-alist)
    (dolist (item calibre-item-list display-alist)
      (setq display-alist
            (cons (list (calibredb-format-item item) item) display-alist)))))

(defun calibredb-candidates()
  "Generate ebooks candidates alist."
  (let* ((query-result (calibredb-query calibredb-query-string))
         (line-list (split-string (calibredb-chomp query-result) "\n")))
    (if (equal "" query-result)
          '("")
      (let (res-list)
        (dolist (line line-list)
          ;; validate if it is right format
          (if (string-match-p "^[0-9]\\{1,10\\}\1" line)
              ;; decode and push to res-list
              (push (calibredb-query-to-alist line) res-list)
            ;; concat the invalid format strings into last line
            (setf (cadr (assoc :comment (car res-list))) (concat (cadr (assoc :comment (car res-list))) line))))
        (calibredb-getbooklist (nreverse res-list))))))

(defun calibredb-helm-read ()
  (helm :sources 'calibredb-helm-source
        :buffer "*helm calibredb*"))

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
  (interactive)
  (if (fboundp 'with-helm-alive-p)
      (with-helm-alive-p
        (helm-exit-and-execute-action #'calibredb-set-metadata--tags)) ))

(defun calibredb-set-metadata--comments-1 ()
  (interactive)
  (if (fboundp 'with-helm-alive-p)
      (with-helm-alive-p
        (helm-exit-and-execute-action #'calibredb-set-metadata--comments)) ))

;; Transient dispatch

(defun calibredb-dispatch nil
  (if (fboundp 'transient-args)
      (transient-args 'calibredb-dispatch)))

;;;###autoload

(defun calibredb-transient ()
  (when (fboundp 'define-transient-command)

    (define-transient-command calibredb-dispatch ()
      "Invoke a calibredb command from a list of available commands."
      ["Metadata"
       [("s" "set_metadata"   calibredb-set-metadata-dispatch)
        ;; ("S" "show_metadata"         calibredb-show-metadata)
        ]]
      ["File operaion"
       [("a" "Add a file"   calibredb-add)]
       [("o" "Open file"         calibredb-find-file)
        ("O" "Open file other frame"            calibredb-find-file-other-frame)]
       [("v" "Open file with default tool"  calibredb-open-file-with-default-tool)]
       [("e" "Export" calibredb-export-dispatch)]]
      (interactive)
      (transient-setup 'calibredb-dispatch))


    (define-transient-command calibredb-set-metadata-dispatch ()
      "Create a new commit or replace an existing commit."
      [["Field"
        ("t" "tags"         calibredb-set-metadata--tags)
        ("c" "comments"         calibredb-set-metadata--comments)]
       ["List fields"
        ("l" "list fileds"         calibredb-set-metadata--list-fields)]]
      (interactive)
      (transient-setup 'calibredb-set-metadata-dispatch))

    (define-transient-command calibredb-export-dispatch ()
      "TODO: Create a new commit or replace an existing commit."
      ;; ["Arguments"
      ;;  ("-a" "Export all books in database, ignoring the list of ids"   ("-a" "--all"))
      ;;  ("-b" "Do not convert non English characters for the file names"  "--dont-asciiize")
      ;;  ("-c" "Do not save cover"   ("-c" " --dont-save-cover"))
      ;;  ("-d" "Do not update metadata"  ("-d" "--dont-update-metadata"))
      ;;  ("-e" "Do not write opf" "--dont-write-opf")
      ;;  ("-f" "Comma separated list of formats to save for each book."  "--formats")
      ;;  ("-g" "Report progress"   ("-g" " --progress"))
      ;;  ("-h" "Replace whitespace with underscores."  ("-h" "--replace-whitespace"))
      ;;  ("-i" "Export all books into a single directory" "--single-dir")
      ;;  ("-k" "Do not convert non English characters for the file names"  "--template")
      ;;  ("-l" "The format in which to display dates. %d - day, %b - month, %m - month number, %Y - year. Default is: %b, %Y"   ("-l" " --timefmt"))
      ;;  ("-m" "Export books to the specified directory. Default is ."  ("-m" "--to-dir"))
      ;;  ("-l" "Convert paths to lowercase." "--to-lowercase")]
      [["Export"
        ("e" "Export"         calibredb-export)]]
      (interactive)
      (transient-setup 'calibredb-export-dispatch))))


(defun calibredb-show-mode ()
  "Mode for displaying book entry details.
\\{calibredb-show-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map calibredb-show-mode-map)
  (setq major-mode 'calibredb-show-mode
        mode-name "calibredb-show"
        buffer-read-only t)
  (buffer-disable-undo)
  (calibredb-transient)
  (run-mode-hooks 'calibredb-show-mode-hook))

(defun calibredb-show--buffer-name (entry)
  "Return the appropriate buffer name for ENTRY.
The result depends on the value of `calibredb-show-unique-buffers'."
  (if calibredb-show-unique-buffers
      (format "*calibredb-entry-<%s>*"
              (calibredb-getattr entry :book-title))
    "*calibredb-entry*"))

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
      (setq calibredb-show-entry entry))
    (funcall calibredb-show-entry-switch buff)))

(defun calibredb-search-buffer ()
  (get-buffer-create "*calibredb-search*"))

(defun calibredb-search-header ()
  "TODO: Returns the string to be used as the Calibredb header.
Indicating the library you use."
  (format "%s %s" "Library: " calibredb-root-dir))

(defun calibredb-search-mode ()
  "Major mode for listing calibre entries.
\\{calibredb-search-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map calibredb-search-mode-map)
  (setq major-mode 'calibredb-search-mode
        mode-name "calibredb-search"
        truncate-lines t
        buffer-read-only t
        header-line-format '(:eval (funcall calibredb-search-header-function)))
  (buffer-disable-undo)
  (set (make-local-variable 'hl-line-face) 'calibredb-search-header-highlight-face)
  (hl-line-mode)
  (run-mode-hooks 'calibredb-search-mode-hook))

(defun calibredb ()
  "Enter calibre Search Buffer."
  (interactive)
  (when (get-buffer (calibredb-search-buffer))
    (kill-buffer (calibredb-search-buffer)))
  (switch-to-buffer (calibredb-search-buffer))
  (goto-char (point-min))
  (dolist (item (calibredb-candidates))
    (let (beg end)
      (setq beg (point))
      ;; (insert (propertize (car item)
      ;;                     ;; 'mouse-face 'mode-line-highlight
      ;;                     'help-echo "mouse-3 or RET to show details"
      ;;                     ;; 'help-echo (calibredb-getattr (cdr item) :book-title)
      ;;                     ))
      (insert (car item))
      (setq end (point))
      ;; (let ((map (make-sparse-keymap)))
      ;;   (define-key map [mouse-3] 'calibredb-search-mouse-3)
      ;;   (define-key map (kbd "<RET>") '(lambda ()
      ;;                                    (interactive)
      ;;                                    (calibredb-show-entry (cdr (get-text-property (point) 'calibredb-entry nil)))))
      ;;   (put-text-property beg end 'keymap map))
      (put-text-property beg end 'calibredb-entry item)
      (insert "\n")))
  (goto-char (point-min))
  (unless (eq major-mode 'calibredb-search-mode)
    (calibredb-search-mode)))

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

(defun calibredb-show-refresh ()
  "Refresh ENTRY in the current buffer."
  (interactive)
  (calibredb-show-entry (get-text-property (point-min) 'calibredb-entry nil)))

(defun calibredb-switch-library ()
  "Swich Calibre Library."
  (interactive)
  (setq calibredb-root-dir (calibredb-complete-file "Quick switch library" calibredb-root-dir))
  (setq calibredb-db-dir (expand-file-name "metadata.db"
                                           calibredb-root-dir)))
(defun calibredb-library-list ()
  "Switch library from variable `calibredb-library-alist'.
If under *calibredb-search* buffer, it will auto refresh after
selecting the new item."
  (interactive)
  (setq calibredb-root-dir (completing-read "Quick switch library: " calibredb-library-alist))
  (setq calibredb-db-dir (expand-file-name "metadata.db"
                                           calibredb-root-dir))
  (if (eq major-mode 'calibredb-search-mode)
      (calibredb))
  (message (concat "Current Library: " calibredb-root-dir)))

(provide 'calibredb)

;;; calibredb.el ends here
