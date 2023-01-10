;;; calibredb-utils.el --- Utils for calibredb -*- lexical-binding: t; -*-

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
(require 'calibredb-search)
(require 'calibredb-faces)
(require 'calibredb-show)
(require 'calibredb-transient)
(require 'calibredb-annotation)

(eval-when-compile (defvar calibredb-search-entries))
(eval-when-compile (defvar calibredb-full-entries))
(eval-when-compile (defvar calibredb-images-path))
(eval-when-compile (defvar calibredb-opds-download-dir))


(declare-function calibredb-search-buffer "calibredb-search.el")
(declare-function calibredb-detailed-view-insert-image "calibredb-utils.el")
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
(declare-function calibredb-opds-download "calibredb-opds.el")
(declare-function calibredb-opds-request-page "calibredb-opds.el")
(declare-function calibredb-opds-search "calibredb-opds.el")
(declare-function calibredb-opds-mailcap-mime-to-extn "calibredb-opds.el")
(declare-function pdf-info-search-string "pdf-info")
(declare-function pdf-info-gettext "pdf-info")
(declare-function djvu-find-file "djvu")
(declare-function djvu-goto-page "djvu")
(declare-function djvu-next-page "djvu")

(defvar djvu-doc-page)

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
              (insert "[[file:" cover "]]"))
            (insert "\n")
            (insert (format "[[file:%s][%s]]" book title))
            (insert "\n")))))
    (when (buffer-live-p occur-buf)
      (switch-to-buffer-other-window occur-buf)
      (read-only-mode)
      (unless (featurep 'org)
        (require 'org))
      (if (fboundp 'org-mode)
          (org-mode))
      (goto-char (point-min)))))

(defun calibredb-open-with-default-tool (filepath)
  "TODO: consolidate default-opener with dispatcher.
Argument FILEPATH is the file path."
  (cond ((eq system-type 'gnu/linux)
         (call-process "xdg-open" nil 0 nil (expand-file-name filepath)))
        ((eq system-type 'windows-nt)
         (w32-shell-execute "open" (expand-file-name filepath)))
        ((eq system-type 'darwin)
         (start-process "shell-process" "*Messages*"
                        "open" (expand-file-name filepath)))
        (t (message "unknown system!?"))))

(defun calibredb-get-file-path (entry &optional prompt)
  "Get file path from a valid candidate ENTRY.
Optional argument PROMPT to Select a format."
  (let ((file-path (calibredb-getattr entry :file-path)))
    (cond ((s-equals? "" file-path) "")         ; no file-path field
          ((file-exists-p file-path) file-path) ; default file-path is a valid file
          ((calibredb-local-file-exists-p entry) (calibredb-local-file entry)) ; valid local file is found
          ((s-contains? "http" file-path) file-path) ; for http link, just return
          (t (if (s-contains? "," (file-name-extension file-path)) ; try to split the extension (for example, it may be epub,pdf) and return the first format
                 (let* ((parent (file-name-directory file-path))
                        (filename (file-name-base file-path))
                        (ext (s-split "," (file-name-extension file-path)))
                        (files (-map (lambda (e) (expand-file-name (concat filename "." e) parent)) ext)))
                   (if calibredb-preferred-format
                       (or (-first (lambda (f) (string= (file-name-extension f) calibredb-preferred-format)) files) (car files))
                     (if prompt
                         (completing-read "Select a format: " files)
                       (car files))))
               file-path)))))           ; if extension does not have comma, at last just retrun it.

(defun calibredb-local-file (entry)
  "Get the local book file based on ENTRY."
  (let* ((book-title (calibredb-getattr entry :book-title))
         (book-format (calibredb-getattr entry :book-format))
         (local-file (expand-file-name (format "%s%s" book-title (calibredb-opds-mailcap-mime-to-extn book-format)) calibredb-opds-download-dir)))
    local-file))

(defun calibredb-local-file-exists-p (entry)
  "Check local book file exists or not based on ENTRY."
  (file-exists-p (calibredb-local-file entry)))

(defun calibredb-get-cover (entry)
  "Get cover path based on ENTRY.
Download it if book-cover is non-nil."
  (let ((file-path (calibredb-getattr entry :file-path))
        (book-format (calibredb-getattr entry :book-format))
        (book-cover (calibredb-getattr entry :book-cover)))
    (pp book-cover)
    (cond ((image-type-available-p (intern book-format)) file-path) ; the file is an image
          ((file-exists-p (concat (file-name-directory file-path) "cover.jpg"))
           (concat (file-name-directory file-path) "cover.jpg")) ; cover.jpg exists
          ((not book-cover)                                      ; book-cover is nil, use default cover
           (expand-file-name "cover.jpg" calibredb-images-path))
          ((s-contains? "base64" book-cover)
           (if (string-match "data:image/\\(.*\\);base64,\\(.*\\)" book-cover)
               (let ((cover (expand-file-name (format "cover.%s" (match-string 1 book-cover)) temporary-file-directory)))
                 (with-current-buffer (generate-new-buffer " *temp*")
                   (insert (base64-decode-string (match-string 2 book-cover)))
                   (write-region (point-min) (point-max) cover))
                 cover)
             (expand-file-name "cover.jpg" calibredb-images-path))) ; TODO: handle base64 cover images
          ((not (s-contains? "base64" book-cover))
           (let* ((library (-first (lambda (lib)
                                     (s-contains? (file-name-directory (car lib)) book-cover))
                                   calibredb-library-alist))
                  (url-request-method "GET")
                  (url-user-agent "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36")
                  (url-request-extra-headers
                   `,(if (and (nth 1 library) (nth 2 library))
                         `(("Content-Type" . "application/xml")
                           ("Authorization" . ,(concat "Basic "
                                                       (base64-encode-string
                                                        (concat (nth 1 library) ":" (nth 2 library))))))
                      '(("Content-Type" . "application/xml"))))
                  (url-automatic-caching t)
                  (filename (url-cache-create-filename book-cover)))
             (if (not (url-is-cached book-cover))
                 (with-current-buffer (url-retrieve-synchronously book-cover)
                   (goto-char (point-min))
                   (search-forward "\n\n")
                   (write-region (point) (point-max) filename)))
             filename))
          (t (expand-file-name "cover.jpg" calibredb-images-path))))) ;return the default image

(defun calibredb-insert-image (path alt width height)
  "Insert an image for PATH at point with max WIDTH and max HEIGTH, falling back to ALT."
  (cond
   ((not (display-graphic-p))
    (insert alt))
   ((fboundp 'imagemagick-types)
    (insert-image
     (create-image path 'imagemagick nil
                   :ascent 100
                   :max-width width
                   :max-height height)))
   (t
    ;; emacs 27.1
    (let ((image (ignore-errors (create-image path nil nil :width width :height nil))))
      (if image
          (insert-image image)
        (insert alt))))))

(defun calibredb-find-file (&optional candidate arg)
  "Open file of the selected item.
If the universal prefix ARG is used, ignore `calibredb-preferred-format'.
Optional argument CANDIDATE is the selected item."
  (interactive
   (list (car (calibredb-find-candidate-at-point))
         current-prefix-arg))
  (let ((file (if current-prefix-arg
                  (let ((calibredb-preferred-format nil))
                    (calibredb-get-file-path candidate t))
                (calibredb-get-file-path candidate t))))
    (cond ((s-contains? "http" file)
           (let ((url (calibredb-getattr candidate :file-path))
                 (title (calibredb-getattr candidate :book-title))
                 (type (calibredb-getattr candidate :book-format)))
             (if (s-equals-p title "search") ; TODO: Workaround, now it only works with calibre-web
                 (calibredb-opds-search calibredb-root-dir)
               (message url)
               (message type)
               (let ((library (-first (lambda (lib)
                                        (s-contains? (file-name-directory (car lib)) url))
                                      calibredb-library-alist)))
                 (if (calibredb-opds-mailcap-mime-to-extn type)
                     (calibredb-opds-download title url (calibredb-opds-mailcap-mime-to-extn type) (nth 1 library) (nth 2 library))
                   (calibredb-opds-request-page url (nth 1 library) (nth 2 library)))))) )
          ((s-equals? "" file) (message "No files."))
          (t (find-file file)))))

(defun calibredb-find-file-other-frame (&optional candidate arg)
  "Open file in other frame of the selected item.
If the universal prefix ARG is used, ignore `calibredb-preferred-format'.
Optional argument CANDIDATE is the selected item."
  (interactive
   (list (car (calibredb-find-candidate-at-point))
         current-prefix-arg))
  (find-file-other-frame (if current-prefix-arg
                             (let ((calibredb-preferred-format nil))
                               (calibredb-get-file-path candidate t))
                             (calibredb-get-file-path candidate t))))

(defun calibredb-open-file-with-default-tool (&optional candidate arg)
  "Open file with the system default tool.
If the universal prefix ARG is used, ignore `calibredb-preferred-format'.
Optional argument CANDIDATE is the selected item."
  (interactive
   (list (car (calibredb-find-candidate-at-point))
         current-prefix-arg))
  (if current-prefix-arg
      (let ((calibredb-preferred-format nil))
        (calibredb-open-with-default-tool (calibredb-get-file-path candidate t)))
    (calibredb-open-with-default-tool (calibredb-get-file-path candidate t))))

(defun calibredb-quick-look (&optional candidate arg)
  "Quick the file with the qlmanage, but it only Support macOS.
If the universal prefix ARG is used, ignore `calibredb-preferred-format'.
Optional argument CANDIDATE is the selected item."
  (interactive
   (list (car (calibredb-find-candidate-at-point))
         current-prefix-arg))
  (let ((file (shell-quote-argument
               (expand-file-name (if current-prefix-arg
                                     (let ((calibredb-preferred-format nil))
                                       (calibredb-get-file-path candidate t))
                                   (calibredb-get-file-path candidate t))))))
    (if (eq system-type 'darwin)
        (call-process-shell-command (concat "qlmanage -p " file) nil 0)
      (message "This feature only supports macOS."))))

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
          (let ((path (calibredb-get-file-path cand t))
                (title (calibredb-getattr cand :book-title)))
            (setq capture-path path)
            (setq capture-title title)))))
    (with-temp-buffer (insert "* TODO ")
                      (insert (format "[[file:%s][%s]]" capture-path capture-title))
                      (buffer-string))))

(defun calibredb-add (arg)
  "Add file(s) into calibredb.
With ivy-mode: Add marked items.
Others: Add only one item.
If prefix ARG is non-nil, keep the files after adding without prompt."
  (interactive "P")
  (cond ((and (boundp 'ivy-mode)
              ivy-mode
              (fboundp 'counsel--find-file-1))
         (counsel--find-file-1
          "Add file(s) to calibre: " calibredb-download-dir
          (lambda (file)
            (calibredb-counsel-add-file-action arg file))
          'calibredb-add))
        (t (let ((file (read-file-name "Add a file to Calibre: " calibredb-download-dir)))
             (calibredb-counsel-add-file-action arg file))))
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
                     :input (format "--add %s" (concat (file-name-as-directory (calibredb-complete-directory-quote "Add a directory to Calibre")) "*"))
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

(defun calibredb-complete-directory-quote (&optional arg &rest rest)
  "Get quoted directory name using completion.
Optional argument ARG is the prompt.
Optional argument REST is the rest."
  (let ((file (read-directory-name (format "%s: " arg) (pop rest))))
    (shell-quote-argument (expand-file-name file))))

;; remove

(defun calibredb-remove (&optional candidate)
  "Remove the item at point.
Optional argument CANDIDATE is the item at point."
  (interactive)
  (unless candidate
    (setq candidate (car (calibredb-find-candidate-at-point))))
  (let ((id (calibredb-getattr candidate :id))
        (title (calibredb-getattr candidate :book-title)))
    (if (yes-or-no-p (concat "Are you sure to move: " id " - " title " to recycle bin?"))
        (calibredb-command :command "remove"
                           :id id
                           :library (format "--library-path %s" (calibredb-root-dir-quote))))
    (cond ((equal major-mode 'calibredb-show-mode)
           (kill-buffer (calibredb-show--buffer-name candidate))
           (calibredb-search-refresh))
          ((eq major-mode 'calibredb-search-mode)
           (calibredb-search-refresh-or-resume)))))

(defun calibredb-remove-marked-items (arg)
  "Remove the marked item(s).
If prefix ARG is non-nil, delete the files without prompt."
  (interactive "P")
  (let ((candidates (calibredb-find-marked-candidates)))
    (unless candidates
      (setq candidates (calibredb-find-candidate-at-point)))
    (let ((ids (mapconcat (lambda (cand) (calibredb-getattr cand :id))  candidates "," )))
      ;; If with prefix, delete without prompt
      (if arg
          (progn
            (calibredb-command :command "remove"
                               :id ids
                               :library (format "--library-path %s" (calibredb-root-dir-quote)))
            (message "Deleted %s" ids))
        (if (yes-or-no-p (concat "Are you sure to move: " ids " to recycle bin?"))
            (calibredb-command :command "remove"
                               :id ids
                               :library (format "--library-path %s" (calibredb-root-dir-quote))))))
    (if (eq major-mode 'calibredb-search-mode)
        (calibredb-search-refresh-or-resume))))

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
        ((equal name "id") (calibredb-getattr cand :id))
        ((equal name "ids") (calibredb-getattr cand :ids))))

(defun calibredb-set-metadata (name &rest props)
  "Set metadata on file NAME on marked candidates.
Argument PROPS are the additional parameters."
  (let ((candidates (plist-get props :candidate)))
    (unless candidates
      (setq candidates (or (calibredb-find-marked-candidates) (calibredb-find-candidate-at-point))))
    (let* ((cands (cond ((memq this-command '(ivy-dispatching-done)) (list candidates))
                        ((memq this-command '(helm-maybe-exit-minibuffer)) (if (fboundp 'helm-marked-candidates)
                                                                               (helm-marked-candidates) nil))
                        (t candidates)))
           (cand (car cands))           ; we use car of cands to get the prompt data
           (title (calibredb-getattr cand :book-title))
           (id (calibredb-getattr cand :id))
           (prompt (plist-get props :prompt))
           (field name)
           (init (calibredb-get-init field cand))
           (num (length cands))
           (input (read-string (if (> num 0)
                                   (concat "Set " field " for " (number-to-string num) " items: ")
                                 (concat prompt id " " title ": ") ) init)))
      (calibredb-set-metadata-process cands field input))))

(defun calibredb-set-metadata-process (cands field input)
  "Run sequential processes to set metadata.
Argument CANDS is the list of candiates.
Argument FIELD is the metadata field, e.g. tags, author.
Argument INPUT is the metadata contents to be set."
  (let ((cand (pop cands)))
    ;; (pp cand)
    (if cand
        (set-process-sentinel
         (let* ((id (calibredb-getattr cand :id)))
           (calibredb-process :command "set_metadata"
                              :option "--field"
                              :input (format "%s:\"%s\"" field input)
                              :id id
                              :library (format "--library-path \"%s\"" calibredb-root-dir)))
         (lambda (p _e)
           (when (= 0 (process-exit-status p))
             (calibredb-set-metadata-process cands field input))))
      ;; if no candidate left to be processed, refresh *calibredb-search*
      (cond ((equal major-mode 'calibredb-show-mode)
             (calibredb-show-refresh))
            ((eq major-mode 'calibredb-search-mode)
             (calibredb-search-refresh-or-resume))
            (t nil)))))


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
    (calibredb-set-metadata--transient-process candidates)))

(defun calibredb-set-metadata--transient-process (cands)
  "Run sequential processes to set metadata with transient commands.
Argument CANDS is the list of candiates."
  (let ((cand (pop cands)))
    ;; (pp cand)
    (if cand
        (set-process-sentinel
         (let* ((id (calibredb-getattr cand :id)))
           (calibredb-process :command "set_metadata"
                              :option (format "--field \"%s\"" (s-join "\" --field \"" (-remove 's-blank? (-flatten (calibredb-set-metadata-arguments)))))
                              :id id
                              :library (format "--library-path \"%s\"" calibredb-root-dir)))
         (lambda (p _e)
           (when (= 0 (process-exit-status p))
             (calibredb-set-metadata--transient-process cands))))
      ;; if no candidate left to be processed, refresh *calibredb-search*
      (cond ((equal major-mode 'calibredb-show-mode)
             (calibredb-show-refresh))
            ((eq major-mode 'calibredb-search-mode)
             (calibredb-search-refresh-or-resume))
            (t nil)))))

(defun calibredb-find-candidate-at-point ()
  "Find candidate at point and return the list."
  (interactive)
  (if (eq major-mode 'calibredb-search-mode)
      (list (cdr (or (get-text-property (point) 'calibredb-entry nil)
                     (get-text-property (point) 'calibredb-detailed nil)
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

(defun calibredb-pdf-auto-detect-isbn (&optional end-page)
  "Invoke from calibre-search buffer.
This function requires the pdf-tools (pdf-tools.el) to be installed.
Scan for isbn from page 1 upto (not including) END-PAGE (default 10) for pdf file."
  (require 'pdf-tools nil t)
  (if (eq major-mode 'calibredb-search-mode)
      (let (isbn-line
            ;; (isbn "")
            (page 1)
            (file-path (calibredb-getattr (car (calibredb-find-candidate-at-point)) :file-path)))
        (unless end-page (setq end-page 10))
        (cond ((string= (url-file-extension file-path) ".pdf")
               (while (< page end-page) ; scanning from below because we want to find first instance of ISBN
                 (let ((match (cdr (assoc 'edges (car (or (pdf-info-search-string
                                                        "isbn"
                                                        page
                                                        file-path)
                                                          (pdf-info-search-string
                                                           "number-"
                                                           page
                                                           file-path)))))))
                   ;; (current-buffer)))))))
                   (setq page (1+ page))
                   (cond (match (setq isbn-line
                                      (pdf-info-gettext
                                       (1- page)
                                       (car match)
                                       'line file-path))
                                (setq page (1+ end-page))))))
               (cond (isbn-line
                      (cond ((string-match "\\(ISBN\\)[^0-9]*\\(10\\|13\\)*[^0-9]* *\\([0-9- x]*\\) *" isbn-line)
                             (match-string 3 isbn-line))
                            ((string-match "13: *\\([0-9- x]*\\) *" isbn-line)
                             (match-string 1 isbn-line))))
                     (t nil)))
              (t nil)))
    (message "Should be invoked from *calibredb-search* buffer")))

(defun calibredb-djvu-auto-detect-isbn ()
  "Invoke from calibre-search buffer.
This function requires the djvu (djvu.el) package to be installed.
Scan for isbn from the first 9 pages of the djvu file."
  (djvu-find-file (calibredb-getattr (car (calibredb-find-candidate-at-point)) :file-path))
  (djvu-goto-page 1)
  (let* ((match (let ((page djvu-doc-page)
                      (match nil))
                  (while (not (or match (> page 10)))
                    (djvu-next-page 1)
                    (setq page djvu-doc-page)
                    (when (re-search-forward "^.*isbn.*$" nil t) (setq match t)))
                  (print match))))
    (let ((isbn-line ""))
      (cond (match
             ;; (print (format "HELLO" (match-string-no-properties 0)))
             (setq isbn-line (match-string-no-properties 0))
             (set-buffer-modified-p nil)
             (kill-buffer)
             (string-match "\\(isbn\\)[^0-9]*\\(10\\|13\\)*[^0-9]* *\\([0-9- x]*\\) *" isbn-line)
             (match-string 3 isbn-line))
            (t
             (set-buffer-modified-p nil)
             (kill-buffer) nil)))))

(defun calibredb-auto-detect-isbn ()
  "Invoke from calibre-search buffer and scan for isbn."
  (interactive)
  (let  ((format (calibredb-getattr (car (calibredb-find-candidate-at-point)) :book-format)))
    (cond ((string= format "pdf")
           (if (featurep 'pdf-tools)
               (calibredb-pdf-auto-detect-isbn)
             nil))
          ((string= format "djvu")
           (if (featurep 'djvu)
               (calibredb-djvu-auto-detect-isbn)
             nil))
          (t nil))))

(defun calibredb-show-results (metadata &optional switch)
  "Display METADATA fetch results in the current buffer.
Optional argument SWITCH to switch to *calibredb-search* buffer to other window.
This function is a slighly modified version from function `calibredb-show-entry'"
  (unless (eq major-mode 'calibredb-show-mode)
    (when (get-buffer (calibredb-show--buffer-name metadata))
      (kill-buffer (calibredb-show--buffer-name metadata))))
  (let* ((buff (get-buffer-create (calibredb-show--buffer-name metadata)))
         (tag (cdr (assoc "Tags" metadata)))
         (comment (cdr (assoc "Comments" metadata)))
         (myauthors (cdr (assoc "Authors" metadata)))
         (title (cdr (assoc "Title" metadata)))
         (pubdate (cdr (assoc "Published" metadata)))
         ;; (query-result (cdr (car (calibredb-candidate id)))) ; get the new metadata through SQL query
         ;; (cover (format "/tmp/%s.jpg" source))
         (cover (calibredb-get-cover (car (calibredb-find-candidate-at-point))))
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
        (insert (format "Author(s)   %s\n" (propertize myauthors 'face 'calibredb-author-face)))
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
          (print "No cover available"))
        ;; (setq end (point))
        (calibredb-show-mode)
        ;; (setq calibredb-show-metadata metadata)
        (goto-char (point-min))))
    (unless (eq major-mode 'calibredb-show-mode)
      (switch-to-buffer buff)
      (when switch
        (switch-to-buffer-other-window (set-buffer (calibredb-search--buffer-name)))
        (goto-char original)))))

(defun calibredb-fetch-metadata-from-sources (author title &optional ids isbn fetch-cover)
  "Fetch metadata from online source via author and title or ISBN.
Invoke from *calibredb-search* buffer.
AUTHOR, TITLE, IDS and ISBN should be strings.
Returns an alist with elements (SOURCE RESULTS) where SOURCE is a
string and RESULTS is an alist with elements (PROP VALUE). If no
metadata was found from a source then in then nil is returned in
the outer alist (nil instead of (SOURCE RESULTS))."
  (let* ((authors (if (or isbn ids) ""
                    (read-string "Authors: " author)))
         (title (if (or isbn ids) ""
                  (read-string "Title: " title)))
         (id (if ids (completing-read "ID: " ids)
               nil))
         (isbn (if isbn (read-string "ISBN: " isbn)
                 nil)))
    (message "Fetching metadata from sources... may take a few seconds")
    (let* ((sources calibredb-fetch-metadata-source-list)
           (results (mapcar
                     (lambda (source)
                       (let* ((cmd (cond
                                    (id
                                     (format
                                      (if fetch-cover
                                          `,(format "%%s -p \"%%s\" --identifier \"%%s\" -c %s  2>/dev/null"
                                                    (expand-file-name "cover.jpg" temporary-file-directory))
                                        "%s -p \"%s\" --identifier \"%s\" 2>/dev/null")
                                      calibredb-fetch-metadata-program
                                      source
                                      id))
                                    (isbn
                                     (format
                                      (if fetch-cover
                                          `,(format "%%s -p \"%%s\" --isbn \"%%s\" -c %s  2>/dev/null"
                                                   (expand-file-name "cover.jpg" temporary-file-directory))
                                        "%s -p \"%s\" --isbn \"%s\" 2>/dev/null")
                                      calibredb-fetch-metadata-program
                                      source
                                      isbn))
                                    (t
                                     (format
                                      (if fetch-cover
                                          `,(format "%%s -p \"%%s\" --authors \"%%s\" --title \"%%s\" -c %s  2>/dev/null"
                                                   (expand-file-name "cover.jpg" temporary-file-directory))
                                        "%s -p \"%s\" --authors \"%s\" --title \"%s\" 2>/dev/null")
                                      calibredb-fetch-metadata-program
                                      source
                                      authors
                                      title))))
                              (md (shell-command-to-string cmd))
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
                              (kovids-magic "%s -c  \"from calibre.ebooks.metadata import *; import sys; print(author_to_author_sort(' '.join(sys.argv[1:])))\" \"%s\"")
                              (author-sort (when (cdr (assoc "Authors" no-comments))
                                             (shell-command-to-string (format
                                                                       kovids-magic
                                                                       calibredb-debug-program
                                                                       (intern (cdr (assoc "Authors" no-comments)))))))
                              (new-comments (when author-sort (append no-comments (list (cons "Author_sort" author-sort))))))
                         (message cmd)
                         (if (nth 1 md-split)
                             (when new-comments (cons source (append new-comments (list (cons "Comments" (substring (nth 1 md-split) 2))))))
                           (when new-comments (cons source new-comments)))))
                     sources)))
      (if (remove nil results)
          (remove nil results)
        nil))))

(defun calibredb-select-and-set-cover ()
  "Select and set cover."
  (when (get-buffer (calibredb-show--buffer-name (calibredb-find-candidate-at-point)))
    (kill-buffer (calibredb-show--buffer-name (calibredb-find-candidate-at-point))))
  (let ((original (calibredb-get-cover (car (calibredb-find-candidate-at-point)))))
    (if (and (file-exists-p original) (file-exists-p (expand-file-name "cover.jpg" temporary-file-directory)))
        (let* ((buff (get-buffer-create (calibredb-show--buffer-name (calibredb-find-candidate-at-point))))
               (fetched (expand-file-name "cover.jpg" temporary-file-directory)))
          (clear-image-cache (expand-file-name "cover.jpg" temporary-file-directory))
          (with-current-buffer buff
            (calibredb-insert-image original "" calibredb-list-view-image-max-width calibredb-list-view-image-max-height)
            (insert " original  fetched ")
            (calibredb-insert-image fetched "" calibredb-list-view-image-max-width calibredb-list-view-image-max-height)
            (switch-to-buffer buff)
            (when (string= (completing-read "Select cover: " '("original" "fetched")) "fetched")
              (rename-file (expand-file-name "cover.jpg" temporary-file-directory) original t))
            (kill-buffer)))
      (cond ((file-exists-p (expand-file-name "cover.jpg" temporary-file-directory))
             (rename-file (expand-file-name "cover.jpg" temporary-file-directory) original t)
             (print "Fetched cover added to entry"))
            (t (print "No cover could be fetched"))))))

(defun calibredb-select-metadata-source (results)
  "Select metadata source.
Argument RESULTS is the source list."
  (cdr (assoc (if (fboundp 'ivy-read)
                  (ivy-read "Select metadata source (preview with C-M-n/p): " results
                            :action
                            (lambda (x) (if calibredb-show-results
                                            (calibredb-show-results (cdr x)))))
                (completing-read "Select metadata source : " results))
              results)))

(defun calibredb-fetch-metadata (author title &optional ids isbn)
  "Fetch metadata.
Argument AUTHOR prompts to input the author.
Argument IDS prompts to input the ids.
Argument TITLE prompts to input the title.
Optional argument ISBN prompts to input the isbn."
  (let* ((fetch-cover (cond ((string= calibredb-fetch-covers "yes") t)
                            ((string= calibredb-fetch-covers "no") nil)
                            (t (yes-or-no-p "Fetch cover?: "))))
         (results (calibredb-fetch-metadata-from-sources author title ids isbn fetch-cover)))
    (cond (results
           (when fetch-cover (calibredb-select-and-set-cover))
           (calibredb-select-metadata-source results))
          (t nil))))

(defun calibredb-fetch-and-set-metadata (type &optional arg)
  "Add metadata from `calibredb-fetch-metadata' to entry at POINT.
Argument TYPE Either 'author' or 'isbn'.
Optional argument ARG."
  (let* ((candidate (car (calibredb-find-candidate-at-point)))
         (id (calibredb-getattr candidate :id))
         (ids (split-string (calibredb-getattr candidate :ids) ","))
         (myauthors (calibredb-getattr candidate :author-sort))
         (title (calibredb-getattr candidate :book-title))
         (metadata
          (cond ((string= type "id") (calibredb-fetch-metadata title myauthors ids))
                ((string= type "author") (if arg (calibredb-fetch-metadata title myauthors)
                                           (calibredb-fetch-metadata myauthors title)))
                ((string= type "isbn") (if arg
                                           (calibredb-fetch-metadata myauthors title nil title)
                                         (calibredb-fetch-metadata
                                          myauthors
                                          title
                                          nil
                                          (cond ((calibredb-auto-detect-isbn))
                                                (""))))))))
    (cond (metadata
           (mapc (lambda (x)
                   (calibredb-command :command "set_metadata"
                                      :option (format "--field %s:%s " (downcase (car x)) (prin1-to-string (cdr x)))
                                      :id id
                                      :library (format "--library-path \"%s\"" calibredb-root-dir)))
                 metadata)
           (let ((window (get-buffer-window "*calibredb-search*")))
             (if window
                 (select-window window)
               (switch-to-buffer-other-window "*calibredb-search*")))
           (calibredb-search-refresh-or-resume)
           (if calibredb-show-results (calibredb-show-results metadata t))
           (message "Metadata updated: ID - %s, Title - %s, Authors - %s." id title myauthors))
          ;; (switch-to-buffer-other-window "*calibredb-entry*"))
          (t (print "No metadata retrieved from sources")))))

(defun calibredb-fetch-and-set-metadata-by-author-and-title (arg)
  "Invoke from *calibredb-search* buffer.
Fetch metadata from online source via author and title. With universal ARG \\[universal-argument] switch initial values of authors and title."
  (interactive "P")
  (calibredb-fetch-and-set-metadata "author" arg))

(defun calibredb-fetch-and-set-metadata-by-id (arg)
  "Invoke from *calibredb-search* buffer.
Fetch metadata from online source via Identifier.
With universal ARG \\[universal-argument] use title as initial value."
  (interactive "P")
  (calibredb-fetch-and-set-metadata "id" arg))

(defun calibredb-fetch-and-set-metadata-by-isbn (arg)
  "Invoke from *calibredb-search* buffer.
Fetch metadata from online source via ISBN.
With universal ARG \\[universal-argument] use title as initial value."
  (interactive "P")
  (calibredb-fetch-and-set-metadata "isbn" arg))

;; show_metadata

(defun calibredb-show-metadata (&optional candidate)
  "Show selected CANDIDATE metadata."
  (interactive)
  (unless candidate
    (if (eq major-mode 'calibredb-search-mode)
        (setq candidate (cdr (get-text-property (point) 'calibredb-entry nil)))
      (setq candidate (get-text-property (point-min) 'calibredb-entry nil))))
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
  (let ((id (calibredb-getattr candidate :id))
        (output-folder (file-name-directory (if (file-executable-p calibredb-device-dir)
                                                (if (yes-or-no-p "Found kindle, do you want to convert and export to kindle?")
                                                    calibredb-device-dir
                                                  (calibredb-complete-file-quote "Export to (select a directory)"))
                                              (calibredb-complete-file-quote "Export to (select a directory)")))))
    (calibredb-command :command "export"
                       :option (s-join " " (-remove 's-blank? (-flatten (calibredb-export-arguments))))
                       :input (format "--to-dir %s" output-folder)
                       :id id
                       :library (format "--library-path %s" (calibredb-root-dir-quote)))))

;; convert ebooks
(defmacro calibredb-convert (type)
  "Macro of function calibredb-convert-to-TYPE."
  `(defun ,(intern (format "calibredb-convert-to-%s" type)) (&optional candidate)
    ,(format "TODO: Convert the slected CANDIDATE to %s." type)
    (interactive)
    (unless candidate
      (if (eq major-mode 'calibredb-search-mode)
          (setq candidate (cdr (get-text-property (point) 'calibredb-entry nil)))
        (setq candidate (get-text-property (point-min) 'calibredb-entry nil))))
    (let (;; (id (calibredb-getattr candidate :id))
          (file (calibredb-get-file-path candidate t))
          (output-folder (file-name-directory (if (file-executable-p calibredb-device-dir)
                                                  (if (yes-or-no-p "Found kindle, do you want to convert and export to kindle?")
                                                      calibredb-device-dir
                                                    (read-file-name "Convert and export to (select a directory): "))
                                                (read-file-name "Convert and export to (select a directory): ")))))
      (set-process-sentinel
       (calibredb-convert-process
        :input (shell-quote-argument (expand-file-name file))
        :output (shell-quote-argument (expand-file-name
                                       (format "%s.%s" (file-name-base file) ,type)
                                       output-folder))
        :option (s-join " " (-remove 's-blank? (-flatten (calibredb-convert-arguments)))))
       (lambda (p _e)
         (when (= 0 (process-exit-status p))
           (message "Conversion finished. Please check logs in *ebook-convert*.")))))) )

(calibredb-convert "azw3")
(calibredb-convert "docx")
(calibredb-convert "epub")
(calibredb-convert "fb2")
(calibredb-convert "html")
(calibredb-convert "htmlz")
(calibredb-convert "lit")
(calibredb-convert "lrf")
(calibredb-convert "mobi")
(calibredb-convert "oeb")
(calibredb-convert "pdb")
(calibredb-convert "pdf")
(calibredb-convert "pml")
(calibredb-convert "rb")
(calibredb-convert "rtf")
(calibredb-convert "snb")
(calibredb-convert "tcr")
(calibredb-convert "txt")
(calibredb-convert "txtz")

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
  (unless calibredb-ref-default-bibliography
    (setq calibredb-ref-default-bibliography
          (concat (file-name-as-directory calibredb-root-dir) "catalog.bib")))
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

(defmacro calibredb-all (field)
  "Macro of function calibredb-all-FIELD."
  `(defun ,(intern (format "calibredb-all-%s" field)) ()
     ,(format "Get all %s and return as a list." field)
     (seq-uniq
      (let (l)
        (cl-loop for entry in calibredb-full-entries do
                 (setq l (append (split-string (calibredb-getattr (cdr entry) ,(intern (format ":%s" field))) ",") l))) l))))

(calibredb-all "tag")
(calibredb-all "id")
(calibredb-all "author-sort")
(calibredb-all "book-dir")
(calibredb-all "book-name")
(calibredb-all "book-format")
(calibredb-all "book-pubdate")
(calibredb-all "book-title")
(calibredb-all "file-path")
(calibredb-all "size")
(calibredb-all "comment")
(calibredb-all "ids")
(calibredb-all "publisher")
(calibredb-all "series")
(calibredb-all "lang_code")
(calibredb-all "last_modified")

(defun calibredb-filter-by-tag ()
  "Filter results by tag."
  (interactive)
  (let ((tag (completing-read "Select tag: " (calibredb-all-tag))))
    (setq calibredb-tag-filter-p t)
    (setq calibredb-favorite-filter-p nil)
    (setq calibredb-author-filter-p nil)
    (setq calibredb-date-filter-p nil)
    (setq calibredb-format-filter-p nil)
    (calibredb-search-keyword-filter tag)))

(defun calibredb-filter-by-author-sort ()
  "Filter results by author-sort."
  (interactive)
  (let ((author (completing-read "Select author: " (calibredb-all-author-sort))))
    (setq calibredb-tag-filter-p nil)
    (setq calibredb-favorite-filter-p nil)
    (setq calibredb-author-filter-p t)
    (setq calibredb-date-filter-p nil)
    (setq calibredb-format-filter-p nil)
    (calibredb-search-keyword-filter author)))

(defun calibredb-filter-by-last_modified ()
  "Filter results by last_modified date."
  (interactive)
  (let ((date (completing-read "Select date: " (seq-uniq (mapcar (lambda (date) (s-left 10 date)) (calibredb-all-last_modified))))))
    (setq calibredb-tag-filter-p nil)
    (setq calibredb-favorite-filter-p nil)
    (setq calibredb-author-filter-p nil)
    (setq calibredb-date-filter-p t)
    (setq calibredb-format-filter-p nil)
    (calibredb-search-keyword-filter date)))


(defun calibredb-filter-by-book-format ()
  "Filter results by book format."
  (interactive)
  (let ((format (completing-read "Select format: " (calibredb-all-book-format))))
    (setq calibredb-tag-filter-p nil)
    (setq calibredb-favorite-filter-p nil)
    (setq calibredb-author-filter-p nil)
    (setq calibredb-date-filter-p nil)
    (setq calibredb-format-filter-p t)
    (calibredb-search-keyword-filter format)))

(defun calibredb-attach-icon-for (path)
  "Return the icon based on PATH."
  (char-to-string
   (pcase (downcase (file-name-extension path))
     ((or "jpg" "jpeg" "png" "gif") ?)
     ("pdf" ?)
     ((or "ppt" "pptx") ?)
     ((or "xls" "xlsx") ?)
     ((or "doc" "docx") ?)
     ((or "ogg" "mp3" "wav" "aiff" "flac") ?)
     ((or "mp4" "mov" "avi") ?)
     ((or "zip" "gz" "tar" "7z" "rar") ?)
     (_ ?))))

(provide 'calibredb-utils)

;;; calibredb-utils.el ends here
