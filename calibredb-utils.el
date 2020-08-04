;;; calibredb-utils.el -*- lexical-binding: t; -*-

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
(require 'calibredb-search)
(require 'calibredb-faces)
(require 'calibredb-show)
(require 'calibredb-transient)
(require 'calibredb-annotation)

(eval-when-compile (defvar calibredb-search-entries))
(eval-when-compile (defvar calibredb-full-entries))
(eval-when-compile (defvar calibredb-images-path))

(declare-function calibredb-search-buffer "calibredb-search.el")
(declare-function calibredb-detail-view-insert-image "calibredb-utils.el")
(declare-function calibredb-search-mode "calibredb-search.el")
(declare-function calibredb-search--buffer-name "calibredb-search.el")
(declare-function calibredb-counsel-add-file-action "calibredb-ivy.el")
(declare-function calibredb-search-refresh-or-resume "calibredb-search.el")
(declare-function calibredb-show--buffer-name "calibredb-show.el")
(declare-function calibredb-search-refresh "calibredb-search.el")
(declare-function calibredb-show-refresh "calibredb-show.el")
(declare-function calibredb-set-metadata-arguments "calibredb-transient.el")
(declare-function calibredb-export-arguments "calibredb-transient.el")
(declare-function calibredb-catalog-bib-arguments "calibredb-transient.el")

;;;###autoload
(defun calibredb-list ()
  "Generate an org buffer which contain all ebooks' cover image, title and the file link."
  (interactive)
  (let* ((buf-name "*calibredb-list*")
         occur-buf)
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (setq occur-buf (get-buffer-create buf-name))
    (let ((res-list (if calibredb-search-entries
                        calibredb-search-entries
                      (progn
                        (setq calibredb-search-entries (calibredb-candidates))
                        (setq calibredb-full-entries calibredb-search-entries)))))
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
          (with-current-buffer occur-buf
            (when (file-exists-p cover)
              (insert "\n")
              (insert "#+attr_org: :width 200px\n")
              (insert (concat "[[file:" cover "]]")))
            (insert "\n")
            (insert (format "[[file:%s][%s]]" book title))
            (insert "\n")))))
    (when (buffer-live-p occur-buf)
      (switch-to-buffer-other-window occur-buf)
      (read-only-mode)
      (org-mode)
      (goto-char (point-min)))))

(defun calibredb-open-with-default-tool (filepath)
  "TODO: consolidate default-opener with dispatcher.
Argument FILEPATH is the file path."
  (if (eq system-type 'windows-nt)
      (start-process "shell-process" "*Messages*"
                     "cmd.exe" "/c" (expand-file-name filepath))
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


(defun calibredb-insert-image (path alt width height)
  "TODO: Insert an image for PATH at point with max WIDTH and max
HEIGTH, falling back to ALT."
  (cond
   ((not (display-graphic-p))
    (insert alt))
   ;; TODO: add native resizing support once it's official
   ((fboundp 'imagemagick-types)
    (insert-image
     (if (file-exists-p path)
         (create-image path 'imagemagick nil
                       :ascent 100
                       :max-width width
                       :max-height height)
       (create-image (expand-file-name "cover.jpg" calibredb-images-path) 'imagemagick nil
                     :ascent 100
                     :max-width width
                     :max-height height))))
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
    (setq candidate (car (calibredb-find-candidate-at-point))))
  (find-file (calibredb-getattr candidate :file-path)))

(defun calibredb-find-file-other-frame (&optional candidate)
  "Open file in other frame of the selected item.
Optional argument CANDIDATE is the selected item."
  (interactive)
  (unless candidate
    (setq candidate (car (calibredb-find-candidate-at-point))))
  (find-file-other-frame (calibredb-getattr candidate :file-path)))

(defun calibredb-open-file-with-default-tool (&optional candidate)
  "Open file with the system default tool.
Optional argument CANDIDATE is the selected item."
  (interactive)
  (unless candidate
    (setq candidate (car (calibredb-find-candidate-at-point))))
  (calibredb-open-with-default-tool (calibredb-getattr candidate :file-path)))

(defun calibredb-read-metadatas (field &optional candidate)
  "Read metadata.
Argument FIELD is the field to read.
Optional argument CANDIDATE is candidate to read."
  (let ((cand))
    (if (eq major-mode 'calibredb-search-mode)
        (if candidate
            (setq cand candidate)
          (setq cand (cdr (get-text-property (point) 'calibredb-entry nil))))
      (if candidate
          (setq cand candidate)
        (setq cand (get-text-property (point-min) 'calibredb-entry nil)) ))
    (calibredb-get-init field cand)))

;; org-capture

(defun calibredb-capture-at-point ()
  "TODO: org capture the current item."
  (interactive)
  (let (capture-path capture-title)
    (with-current-buffer (calibredb-search--buffer-name)
      (let ((candidates (calibredb-find-marked-candidates)))
        (unless candidates
          (setq candidates (calibredb-find-candidate-at-point)))
        (dolist (cand candidates)
          (let ((path (calibredb-getattr cand :file-path))
                (title (calibredb-getattr cand :book-title)))
            (setq capture-path path)
            (setq capture-title title)))))
    (with-temp-buffer (insert "* TODO ")
                      (insert (format "[[file:%s][%s]]" capture-path capture-title))
                      (buffer-string))))

(defun calibredb-open-dired (&optional candidate)
  "Open dired of the selected item.
Optional argument CANDIDATE is the selected item.
Opens a dired buffer in FILE's directory.  If FILE is a
directory, open this directory."
  (interactive)
  (unless candidate
    (setq candidate (car (calibredb-find-candidate-at-point))))
  (let ((file (calibredb-getattr candidate :file-path)))
    (if (file-directory-p file)
        (dired file)
      (dired (file-name-directory file))
      (dired-goto-file file))))

(defun calibredb-add ()
  "Add file(s) into calibredb.
With ivy-mode: Add marked items.
Others: Add only one item."
  (interactive)
  (cond ((if (boundp 'ivy-mode)
             (if ivy-mode
                 (if (fboundp 'counsel--find-file-1)
                     (counsel--find-file-1
                      "Add file(s) to calibredb: " nil
                      #'calibredb-counsel-add-file-action
                      'calibredb-add)))))
        (t
         (calibredb-command :command "add"
                            :input (calibredb-complete-file-quote "Add a file to Calibre")
                            :library (format "--library-path %s" (calibredb-root-dir-quote)))))
  (if (equal major-mode 'calibredb-search-mode)
      (calibredb-search-refresh-or-resume)))

(defun calibredb-add-format (&optional candidate)
  "Add format to selected item.
Optional argument CANDIDATE is the selected item."
  (interactive)
  (unless candidate
    (setq candidate (car (calibredb-find-candidate-at-point))))
  (calibredb-command :command "add_format"
                     :input (concat (calibredb-getattr candidate :id) " " (calibredb-complete-file-quote "Add format to selected item") )
                     :library (format "--library-path %s" (calibredb-root-dir-quote)))
  (if (equal major-mode 'calibredb-search-mode)
      (calibredb-search-refresh-or-resume)))

(defun calibredb-add-dir (&optional option)
  "Add all files in a directory into calibre database.
By default only files that have extensions of known e-book file
types are added.
Optional argument OPTION is additional options."
  (interactive)
  (calibredb-command :command "add"
                     :input (format "--add %s" (concat (file-name-as-directory (calibredb-complete-file-quote "Add a directory to Calibre")) "*"))
                     :option (or option "")
                     :library (format "--library-path %s" (calibredb-root-dir-quote)))
  (if (equal major-mode 'calibredb-search-mode)
      (calibredb-search-refresh-or-resume)))

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
    (setq candidate (car (calibredb-find-candidate-at-point))))
  (let ((id (calibredb-getattr candidate :id))
        (title (calibredb-getattr candidate :book-title)))
    (if (yes-or-no-p (concat "Confirm Delete: " id " - " title))
        (calibredb-command :command "remove"
                           :id id
                           :library (format "--library-path %s" (calibredb-root-dir-quote))))
    (cond ((equal major-mode 'calibredb-show-mode)
           (kill-buffer (calibredb-show--buffer-name candidate))
           (calibredb-search-refresh))
          ((eq major-mode 'calibredb-search-mode)
           (calibredb-search-refresh-or-resume)))))

(defun calibredb-remove-format (&optional candidate)
  "Remove the slected format.
Optional argument CANDIDATE is the selected item."
  (interactive)
  (unless candidate
    (setq candidate (car (calibredb-find-candidate-at-point))))
  (let ((id (calibredb-getattr candidate :id))
        (format (calibredb-getattr candidate :book-format))
        (title (calibredb-getattr candidate :book-title)))
    (if (yes-or-no-p (concat "Confirm Delete: id - " id ", title - " title ", format - " format))
        (calibredb-command :command "remove_format"
                           :id (concat id " " format)
                           :library (format "--library-path %s" (calibredb-root-dir-quote))))
    (cond ((equal major-mode 'calibredb-show-mode)
           (kill-buffer (calibredb-show--buffer-name candidate))
           (calibredb-search-refresh))
          ((eq major-mode 'calibredb-search-mode)
           (calibredb-search-refresh-or-resume)))))

;; set_metadata

(defun calibredb-get-init (name cand)
  "Get the initial value in completing prompt.
Argument NAME is the metadata field name string.
Argument CAND is the candidate."
  (cond ((equal name "tags") (calibredb-getattr cand :tag))
        ((equal name "comments") (calibredb-getattr cand :comment))
        ((equal name "author_sort") (calibredb-getattr cand :author-sort))
        ((equal name "authors") (calibredb-getattr cand :author-sort))
        ((equal name "title") (calibredb-getattr cand :book-title))
        ((equal name "id") (calibredb-getattr cand :id))))

(defun calibredb-set-metadata (name &rest props)
  "Set metadata on file NAME on marked candidates.
Argument PROPS are the additional parameters."
  (let ((candidates (plist-get props :candidate)))
    (unless candidates
      (setq candidates (or (calibredb-find-marked-candidates) (calibredb-find-candidate-at-point))))
    (let ((last-input))
      (dolist (cand (cond ((memq this-command '(ivy-dispatching-done)) (list candidates))
                          ((memq this-command '(helm-maybe-exit-minibuffer)) (if (fboundp 'helm-marked-candidates)
                                                                                 (helm-marked-candidates) nil))
                          (t candidates)))
        (let* ((title (calibredb-getattr cand :book-title))
               (id (calibredb-getattr cand :id))
               (prompt (plist-get props :prompt))
               (field name)
               (init (calibredb-get-init field cand))
               (num (length (calibredb-find-marked-candidates)))
               (input (or last-input (read-string (if (> num 0)
                                                      (concat "Set " field " for " (number-to-string num) " items: ")
                                                    (concat prompt id " " title ": ") ) init))))
          (calibredb-command :command "set_metadata"
                             :option "--field"
                             :input (format "%s:\"%s\"" field input)
                             :id id
                             :library (format "--library-path \"%s\"" calibredb-root-dir))
          ;; set the input as last input, so that all items use the same input
          (setq last-input input)
          (cond ((equal major-mode 'calibredb-show-mode)
                 (calibredb-show-refresh))
                ((eq major-mode 'calibredb-search-mode)
                 (calibredb-search-refresh-or-resume))
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
  "Change author_sort on marked CANDIDATEs."
  (interactive)
  (calibredb-set-metadata "author_sort"
                          :prompt "Change author for "
                          :candidate candidate))

(defun calibredb-set-metadata--authors (&optional candidate)
  "Change authors on marked CANDIDATEs."
  (interactive)
  (calibredb-set-metadata "authors"
                          :prompt "Change author for "
                          :candidate candidate))

(defun calibredb-set-metadata--list-fields (&optional candidate)
  "List the selected CANDIDATE supported fileds."
  (interactive)
  (unless candidate
    (setq candidate (car (calibredb-find-candidate-at-point))))
  (let* ((id (calibredb-getattr candidate :id)))
    (message (calibredb-command :command "set_metadata"
                                :option "--list-fields"
                                :id id
                                :library (format "--library-path %s" (calibredb-root-dir-quote))))))

(defun calibredb-set-metadata--transient ()
  "Set metadata for candidate at point or marked candidates with transient arguments."
  (interactive)
  (let ((candidates (calibredb-find-marked-candidates)))
    (unless candidates
      (setq candidates (calibredb-find-candidate-at-point)))
    (dolist (cand candidates)
      (let ((id (calibredb-getattr cand :id)))
        (calibredb-command :command "set_metadata"
                           :option (format "--field \"%s\"" (s-join "\" --field \"" (-remove 's-blank? (-flatten (calibredb-set-metadata-arguments)))))
                           :id id
                           :library (format "--library-path \"%s\"" calibredb-root-dir))
        (cond ((equal major-mode 'calibredb-show-mode)
               (calibredb-show-refresh))
              ((eq major-mode 'calibredb-search-mode)
               (calibredb-search-refresh-or-resume))
              (t nil))))))

(defun calibredb-find-candidate-at-point ()
  "Find candidate at point and return the list."
  (interactive)
  (if (eq major-mode 'calibredb-search-mode)
      (list (cdr (or (get-text-property (point) 'calibredb-entry nil)
                     (get-text-property (point) 'calibredb-detail nil)
                     (get-text-property (point) 'calibredb-compact nil))))
    (list (get-text-property (point-min) 'calibredb-entry nil) )))

(defun calibredb-find-marked-candidates ()
  "Find marked candidates and return the alist."
  (interactive)
  (save-excursion
    (let (candidate beg end cand-list)
      (when (text-property-not-all (point-min) (point-max) 'calibredb-mark nil)
        (setq end (text-property-any (point-min) (point-max) 'calibredb-mark ?>))
        (while (setq beg (text-property-any end (point-max) 'calibredb-mark ?>) )
          (goto-char beg)
          (setq candidate (cdr (get-text-property (point) 'calibredb-entry nil)))
          (push candidate cand-list)
          ;; (message (number-to-string beg))
          (forward-line 1)
          (setq end (point)))
        cand-list))))

;; fetch_metadata

(defun calibredb-show-results (metadata &optional switch)
  "Display METADATA fetch results in the current buffer.
Optional argument SWITCH to switch to *calibredb-search* buffer to other window.
This function is a slighly modified version from calibredb-show-entry"
  (unless (eq major-mode 'calibredb-show-mode)
    (when (get-buffer (calibredb-show--buffer-name metadata))
      (kill-buffer (calibredb-show--buffer-name metadata))))
  (let* ((buff (get-buffer-create (calibredb-show--buffer-name metadata)))
         (tag (cdr (assoc "Tags" metadata)))
         (comment (cdr (assoc "Comments" metadata)))
         (authors (cdr (assoc "Authors" metadata)))
         (title (cdr (assoc "Title" metadata)))
         (pubdate (cdr (assoc "Published" metadata)))
         ;; (query-result (cdr (car (calibredb-candidate id)))) ; get the new metadata through SQL query
         ;; (cover (format "/tmp/%s.jpg" source))
         (cover (concat (file-name-directory (calibredb-getattr (car (calibredb-find-candidate-at-point)) :file-path)) "cover.jpg"))
         ;; (format (calibredb-getattr metadata :book-format))
         (original (point))
         beg end)
    (clear-image-cache cover)
    (let ((inhibit-read-only t))
      (with-current-buffer buff
        (erase-buffer)
        (setq beg (point))
        ;; (insert (propertize (calibredb-show-metadata metadata) 'calibredb-metadata metadata))
        (setq end (point))
        (put-text-property beg end 'calibredb-metadata metadata)
        (insert (format "Title       %s\n" (propertize title 'face 'calibredb-title-face)))
        (insert (format "Author(s)   %s\n" (propertize authors 'face 'calibredb-author-face)))
        (when tag (insert (format "Tags        %s\n" (propertize tag 'face 'calibredb-tag-face))))
        (when comment
          (insert (format "Comments    %s\n" (propertize comment 'face 'calibredb-comment-face))))
        (when pubdate
          (insert (format "Published   %s\n" (propertize pubdate 'face 'calibredb-pubdate-face))))
        (insert "\n")
        ;; (if (image-type-available-p (intern format))
        ;;     (calibredb-insert-image file "" calibredb-list-view-image-max-width calibredb-list-view-image-max-height)
        ;;   (calibredb-insert-image cover "" calibredb-list-view-image-max-width calibredb-list-view-image-max-height))
        (if cover
            (calibredb-insert-image cover
                                    ""
                                    calibredb-list-view-image-max-width
                                    calibredb-list-view-image-max-height)
          (print "Entry path was moved or no cover fetched"))
        ;; (setq end (point))
        (calibredb-show-mode)
        (setq calibredb-show-metadata metadata)
        (goto-char (point-min))))
    (unless (eq major-mode 'calibredb-show-mode)
      (switch-to-buffer buff)
      (when switch
        (switch-to-buffer-other-window (set-buffer (calibredb-search--buffer-name)))
        (goto-char original)))))

(defun calibredb-fetch-metadata-from-sources (author title &optional isbn)
  "Fetch metadata from online source via author and title or
ISBN. Invoke from *calibredb-search* buffer.

AUTHOR, TITLE and ISBN should be strings.

Returns an alist with elements (SOURCE RESULTS) where SOURCE is a
string and RESULTS is an alist with elements (PROP VALUE). If no
metadata was found from a source then in then nil is returned in
the outer alist (nil instead of (SOURCE RESULTS))."
  (let* ((authors (if isbn ""
                    (read-string "Authors: " author)))
         (title (if isbn ""
                  (read-string "Title: " title)))
         (isbn (if isbn (read-string "ISBN: " isbn)
                 nil))
         (sources '("Google" "Amazon.com"))
         (results (mapcar
                   (lambda (source)
                     (let* ((md (shell-command-to-string
                                 (if isbn (format
                                           "fetch-ebook-metadata -p '%s' --isbn '%s' -c /tmp/cover.jpg"
                                           source
                                           isbn)
                                   (format
                                    "fetch-ebook-metadata -p '%s' --authors '%s' --title '%s' -c /tmp/cover.jpg"
                                    source
                                    authors
                                    title))))
                            (md-split (if (string-match "No results found$" md) nil
                                        (split-string md "Comments" nil " *")))
                            (no-comments (if md-split
                                             (mapcar (lambda (x)
                                                       (let ((string x))
                                                         (string-match "\\([A-z]*\\)(*\\(s\\)*)* *: *\\(.*\\)" string)
                                                         (cons (format "%s%s" (match-string 1 string) (cond ((match-string 2 string))
                                                                                                            ("")))
                                                               (match-string 3 string))))
                                                     (split-string (car md-split) "\n" t " *"))
                                           nil))
                            (kovids-magic "calibre-debug -c  \"from calibre.ebooks.metadata import *; import sys; print(author_to_author_sort(' '.join(sys.argv[1:])))\" '%s'")
                            (author-sort (shell-command-to-string (format
                                                                   kovids-magic
                                                                   (intern (cdr (assoc "Authors" no-comments))))))
                            (new-comments (append no-comments (list (cons "Author_sort" author-sort)))))
                       (if (nth 1 md-split)
                           (when new-comments (cons source (append new-comments (list (cons "Comments" (substring (nth 1 md-split) 2))))))
                         (when new-comments (cons source new-comments)))))
                   sources)))
    (if (remove nil results)
        (remove nil results)
      nil)
    ))

(defun calibredb-select-and-set-cover (results &optional cover)
  (when (get-buffer (calibredb-show--buffer-name (calibredb-find-candidate-at-point)))
    (kill-buffer (calibredb-show--buffer-name (calibredb-find-candidate-at-point))))
  (let ((original (concat
                   (file-name-directory (calibredb-getattr (car (calibredb-find-candidate-at-point)) :file-path))
                   "cover.jpg")))
    (if (and (file-exists-p original) (file-exists-p "/tmp/cover.jpg"))
        (let* ((buff (get-buffer-create (calibredb-show--buffer-name (calibredb-find-candidate-at-point))))
               (fetched "/tmp/cover.jpg"))
          (clear-image-cache "/tmp/cover.jpg")
          (with-current-buffer buff
            (calibredb-insert-image original "" calibredb-list-view-image-max-width calibredb-list-view-image-max-height)
            (insert " original  fetched ")
            (calibredb-insert-image fetched "" calibredb-list-view-image-max-width calibredb-list-view-image-max-height)
            (switch-to-buffer buff)
            (when (string= (completing-read "Select cover: " '("original" "fetched")) "fetched")
              (rename-file "/tmp/cover.jpg" original t))
            (kill-current-buffer)))
      (cond ((file-exists-p "/tmp/cover.jpg")
             (rename-file "/tmp/cover.jpg" original t)
             (print "Fetched cover added to entry"))
            (t (print "No cover could be fetched"))))))

(defun calibredb-select-metadata-source (results)
  (cdr (assoc (if (fboundp 'ivy-read)
                  (ivy-read "Select metadata source (preview with C-M-n/p): " results
                            :action
                            (lambda (x) (calibredb-show-results (cdr x))))
                (completing-read "Select metadata source : " results))
              results)))
;;)

(defun calibredb-fetch-metadata (author title &optional isbn)
  (let ((results (calibredb-fetch-metadata-from-sources author title isbn)))
    (cond (results (calibredb-select-and-set-cover results) (calibredb-select-metadata-source results))
          (t nil))))

(defun calibredb-fetch-and-set-metadata (type &optional arg)
  "Add metadata from calibredb-fetch-metadata to entry at POINT"
  (let* ((candidate (car (calibredb-find-candidate-at-point)))
         (authors (calibredb-getattr candidate :author-sort))
         (title (calibredb-getattr candidate :book-title))
         (metadata
          (cond ((string= type "author") (if arg (calibredb-fetch-metadata title authors)
                                           (calibredb-fetch-metadata authors title)))
                ((string= type "isbn") (if arg
                                           (calibredb-fetch-metadata authors title title)
                                         (calibredb-fetch-metadata authors title "")))))
         (id (calibredb-getattr candidate :id)))
    (cond (metadata
           (mapcar (lambda (x)
                     (calibredb-command :command "set_metadata"
                                        :option "--field"
                                        :input (format "%s:\"%s\"" (downcase (car x)) (cdr x))
                                        :id id
                                        :library (format "--library-path \"%s\"" calibredb-root-dir)))
                   metadata)
           (switch-to-buffer-other-window "*calibredb-search*")
           (calibredb-search-refresh-or-resume)
           (calibredb-show-results metadata t))
          ;; (switch-to-buffer-other-window "*calibredb-entry*"))
          (t (print "No metadata retrieved from sources")))))

(defun calibredb-fetch-and-set-metadata-by-author-and-title (arg)
  "Invoke from *calibredb-search* buffer. Fetch metadata from
online source via author and title. With universal arg (C-u)
switch initial values of authors and title."
  (interactive "P")
  (calibredb-fetch-and-set-metadata "author" arg))

(defun calibredb-fetch-and-set-metadata-by-isbn (arg)
  "Invoke from *calibredb-search* buffer. Fetch metadata from
online source via ISBN. With universal arg (C-u) use title as
initial value."
  (interactive "P")
  (calibredb-fetch-and-set-metadata "isbn"))

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

;; catalog

(defun calibredb-catalog ()
  "TODO Export the catalog."
  (interactive)
  (calibredb-command :command "catalog"
                     ;; :option (s-join " " (-remove 's-blank? (-flatten (calibredb-export-arguments))))
                     :input (format "%s" (calibredb-complete-file-quote "Export to (select a path)"))
                     ;; :id id
                     :library (format "--library-path %s" (calibredb-root-dir-quote))))

(defun calibredb-catalog-bib--transient ()
  "Export the catalog with BibTex file."
  (interactive)
  (calibredb-command :command "catalog"
                     :option (format "%s"
                                     (shell-quote-argument
                                      (expand-file-name
                                       (or calibredb-ref-default-bibliography
                                           (concat (file-name-as-directory calibredb-root-dir) "catalog.bib")))))
                     :input (s-join " " (-remove 's-blank? (-flatten (calibredb-catalog-bib-arguments))))
                     :library (format "--library-path %s" (calibredb-root-dir-quote)))
  (calibredb-ref-default-bibliography)
  (message "Updated BibTex file."))

(defun calibredb-find-bib ()
  "Open the catalog BibTex file."
  (interactive)
  (if (file-exists-p calibredb-ref-default-bibliography)
      (find-file calibredb-ref-default-bibliography)
    (message "NO BibTex file.")))

(defun calibredb-ref-default-bibliography ()
  "Update the path of BibTex file."
  (setq calibredb-ref-default-bibliography
        (concat (file-name-as-directory calibredb-root-dir) "catalog.bib"))
  (if (boundp 'org-ref-default-bibliography)
      (if (file-exists-p calibredb-ref-default-bibliography)
          (add-to-list 'org-ref-default-bibliography calibredb-ref-default-bibliography))))

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

(provide 'calibredb-utils)

;;; calibredb-utils.el ends here
