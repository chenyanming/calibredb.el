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
(defun calibredb ()
  "Enter calibre Search Buffer."
  (interactive)
  (let ((cand (if calibredb-search-entries
                  calibredb-search-entries
                (progn
                  (setq calibredb-search-entries (calibredb-candidates))
                  (setq calibredb-full-entries calibredb-search-entries)))))
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
                 (calibredb-detail-view-insert-image item)
                 (setq end (point))
                 (put-text-property beg end 'calibredb-entry item)
                 (insert "\n")))
             (goto-char (point-min)))
           (calibredb-ref-default-bibliography)
           (unless (eq major-mode 'calibredb-search-mode)
             (calibredb-search-mode))))))

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
  "Set metadata on filed NAME on amrked candidates.
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
      (list (cdr (get-text-property (point) 'calibredb-entry nil)))
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
