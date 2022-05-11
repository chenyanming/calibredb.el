;;; calibredb-search.el --- Books search buffer for calibredb -*- lexical-binding: t; -*-

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

(eval-when-compile (defvar calibredb-show-entry))
(eval-when-compile (defvar calibredb-show-entry-switch))
(eval-when-compile (defvar calibredb-virtual-library-alist))

(declare-function calibredb "calibredb.el")
(declare-function calibredb-find-file "calibredb-utils.el")
(declare-function calibredb-add "calibredb-utils.el")
(declare-function calibredb-add-dir "calibredb-utils.el")
(declare-function calibredb-clone "calibredb-utils.el")
(declare-function calibredb-remove "calibredb-utils.el")
(declare-function calibredb-remove-marked-items "calibredb-utils.el")
(declare-function calibredb-switch-library "calibredb-library.el")
(declare-function calibredb-library-list "calibredb-library.el")
(declare-function calibredb-library-next "calibredb-library.el")
(declare-function calibredb-library-previous "calibredb-library.el")
(declare-function calibredb-set-metadata-dispatch "calibredb-transient.el")
(declare-function calibredb-find-file-other-frame "calibredb-utils.el")
(declare-function calibredb-open-file-with-default-tool "calibredb-utils.el")
(declare-function calibredb-open-dired "calibredb-utils.el")
(declare-function calibredb-catalog-bib-dispatch "calibredb-transient.el")
(declare-function calibredb-export-dispatch "calibredb-transient.el")
(declare-function calibredb-edit-annotation "calibredb-annotation.el")
(declare-function calibredb-set-metadata--tags "calibredb-utils.el")
(declare-function calibredb-set-metadata--author_sort "calibredb-utils.el")
(declare-function calibredb-set-metadata--authors "calibredb-utils.el")
(declare-function calibredb-set-metadata--title "calibredb-utils.el")
(declare-function calibredb-set-metadata--comments "calibredb-utils.el")
(declare-function calibredb-edit-annotation-header "calibredb-annotation.el")
(declare-function calibredb-show--buffer-name "calibredb-show.el")
(declare-function calibredb-insert-image "calibredb-utils.el")
(declare-function calibredb-show-mode "calibredb-show.el")
(declare-function calibredb-find-marked-candidates "calibredb-utils.el")
(declare-function calibredb-read-metadatas "calibredb-utils.el")
(declare-function calibredb-find-candidate-at-point "calibredb-utils.el")
(declare-function calibredb-show-refresh "calibredb-show.el")
(declare-function calibredb-get-init "calibredb-utils.el")
(declare-function calibredb-virtual-library-list "calibredb-library.el")
(declare-function calibredb-virtual-library-next "calibredb-library.el")
(declare-function calibredb-virtual-library-previous "calibredb-library.el")

(defcustom calibredb-search-filter ""
  "Query string filtering shown entries."
  :group 'calibredb
  :type 'string)

(defvar calibredb-full-entries nil
  "List of the all entries currently on library.")

(defvar calibredb-search-entries nil
  "List of the entries currently on display.")

(defvar calibredb-search-filter-active nil
  "When non-nil, calibredb is currently reading a filter from the minibuffer.
When live editing the filter, it is bound to :live.")

(defvar calibredb-search-last-update 0
  "The last time the buffer was redrawn in epoch seconds.")

(defvar calibredb-search-print-entry-function #'calibredb-search-print-entry--default
  "Function to print entries into the *calibredb-search* buffer.")

(defvar calibredb-tag-filter-p nil)
(defvar calibredb-favorite-filter-p nil)
(defvar calibredb-author-filter-p nil)
(defvar calibredb-date-filter-p nil)
(defvar calibredb-format-filter-p nil)

(defvar calibredb-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-3] #'calibredb-search-mouse)
    (define-key map (kbd "<RET>") #'calibredb-find-file)
    (define-key map "?" #'calibredb-dispatch)
    (define-key map "a" #'calibredb-add)
    (define-key map "A" #'calibredb-add-dir)
    (define-key map "c" #'calibredb-clone)
    (define-key map "d" #'calibredb-remove)
    (define-key map "D" #'calibredb-remove-marked-items)
    (define-key map "j" #'calibredb-next-entry)
    (define-key map "k" #'calibredb-previous-entry)
    (define-key map "l" #'calibredb-virtual-library-list)
    (define-key map "L" #'calibredb-library-list)
    (define-key map "n" #'calibredb-virtual-library-next)
    (define-key map "N" #'calibredb-library-next)
    (define-key map "p" #'calibredb-virtual-library-previous)
    (define-key map "P" #'calibredb-library-previous)
    (define-key map "s" #'calibredb-set-metadata-dispatch)
    (define-key map "S" #'calibredb-switch-library)
    (define-key map "g" #'calibredb-filter-dispatch)
    (define-key map "o" #'calibredb-sort-dispatch)
    (define-key map "O" #'calibredb-find-file-other-frame)
    (define-key map "v" #'calibredb-view)
    (define-key map "V" #'calibredb-open-file-with-default-tool)
    (define-key map "," #'calibredb-quick-look)
    (define-key map "." #'calibredb-dired-open)
    (define-key map "y" #'calibredb-yank-dispatch)
    (define-key map "b" #'calibredb-catalog-bib-dispatch)
    (define-key map "e" #'calibredb-export-dispatch)
    (define-key map "r" #'calibredb-search-refresh-and-clear-filter)
    (define-key map "R" #'calibredb-search-clear-filter)
    (define-key map "q" #'calibredb-search-quit)
    (define-key map "m" #'calibredb-mark-and-forward)
    (define-key map "*" #'calibredb-toggle-favorite-at-point)
    (define-key map "x" #'calibredb-toggle-archive-at-point)
    (define-key map "h" #'calibredb-toggle-highlight-at-point)
    (define-key map "u" #'calibredb-unmark-and-forward)
    (define-key map "i" #'calibredb-edit-annotation)
    (define-key map (kbd "<DEL>") #'calibredb-unmark-and-backward)
    (define-key map (kbd "<backtab>") #'calibredb-toggle-view)
    (define-key map (kbd "TAB") #'calibredb-toggle-view-at-point)
    (define-key map "\M-n" #'calibredb-show-next-entry)
    (define-key map "\M-p" #'calibredb-show-previous-entry)
    (define-key map "/" #'calibredb-search-live-filter)
    (define-key map "\M-t" #'calibredb-set-metadata--tags)
    (define-key map "\M-a" #'calibredb-set-metadata--author_sort)
    (define-key map "\M-A" #'calibredb-set-metadata--authors)
    (define-key map "\M-T" #'calibredb-set-metadata--title)
    (define-key map "\M-c" #'calibredb-set-metadata--comments)
    map)
  "Keymap for `calibredb-search-mode'.")

(defvar calibredb-edit-annotation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'calibredb-send-edited-annotation)
    (define-key map "\C-c\C-k" 'calibredb-annotation-quit)
    map)
  "Keymap for `calibredb-edit-annotation-mode'.")

(defvar calibredb-search-header-function #'calibredb-search-header
  "Function that returns the string to be used for the Calibredb search header.")

(defvar calibredb-images-path (concat (file-name-directory load-file-name) "img")
  "Relative path to images.")

(defcustom calibredb-search-unique-buffers nil
  "TODO: When non-nil, every entry buffer gets a unique name.
This allows for displaying multiple serch buffers at the same
time."
  :group 'calibredb
  :type 'boolean)

(define-obsolete-function-alias #'calibredb-search-ret
  'calibredb-view "calibredb 2.0.0")

(defcustom calibredb-detailed-view nil
  "Set Non-nil to change detail view, nil to compact view - *calibredb-search*."
  :group 'calibredb
  :type 'boolean)

(define-obsolete-variable-alias 'calibredb-detial-view 'calibredb-detailed-view
  "See https://github.com/chenyanming/calibredb.el/pull/45" "Fixing typos.")

(defcustom calibredb-detailed-view-image-show t
  "Set Non-nil to show images in detailed view - *calibredb-search*."
  :group 'calibredb
  :type 'boolean)

(defcustom calibredb-detailed-view-image-max-width 250
  "Max Width for images in detailed view - *calibredb-search*.
For Emacs 27.1+, if imagemagick is disabled, it would the image width."
  :group 'calibredb
  :type 'integer)

(defcustom calibredb-detailed-view-image-max-height 250
  "Max height for images in detailed view - *calibredb-search*.
For Emacs 27.1+, if imagemagick is disabled, the image height is ignored."
  :group 'calibredb
  :type 'integer)

(defcustom calibredb-list-view-image-max-width 500
  "Max Width for images in list view - *calibredb-list*.
For Emacs 27.1+, if imagemagick is disabled, it is the image width."
  :group 'calibredb
  :type 'integer)

(defcustom calibredb-list-view-image-max-height 500
  "Max height for images in list view - *calibredb-list*.
For Emacs 27.1+, if imagemagick is disabled, the image height is ignored."
  :group 'calibredb
  :type 'integer)

(defun calibredb-search--buffer-name ()
  "Return the appropriate buffer name for ENTRY.
The result depends on the value of `calibredb-search-unique-buffers'."
  (if calibredb-search-unique-buffers
      (format "*calibredb-search-<%s>*" calibredb-root-dir)
    "*calibredb-search*"))

(defun calibredb-show-entry (entry &optional switch)
  "Display ENTRY in the current buffer.
Optional argument SWITCH to switch to *calibredb-search* buffer to other window."
  (unless (eq major-mode 'calibredb-show-mode)
      (when (get-buffer (calibredb-show--buffer-name entry))
        (kill-buffer (calibredb-show--buffer-name entry))))
  (let* ((buff (get-buffer-create (calibredb-show--buffer-name entry)))
         (id (calibredb-getattr entry :id)) ; only get the id
         (tag (calibredb-getattr entry :tag))
         (comment (calibredb-getattr entry :comment))
         (author-sort (calibredb-getattr entry :author-sort))
         (title (calibredb-getattr entry :book-title))
         (pubdate (calibredb-getattr entry :book-pubdate))
         ;; (query-result (cdr (car (calibredb-candidate id)))) ; get the new entry through SQL query
         (file (calibredb-getattr entry :file-path))
         (cover (calibredb-get-cover entry))
         (format (calibredb-getattr entry :book-format))
         (size (calibredb-getattr entry :size))
         (ids (calibredb-getattr entry :ids))
         (publisher (calibredb-getattr entry :publisher))
         (series (calibredb-getattr entry :series))
         (lang_code (calibredb-getattr entry :lang_code))
         (last_modified (calibredb-getattr entry :last_modified))
         (original (point))
         (file-map (make-sparse-keymap))
         beg end)
    (let ((inhibit-read-only t) c-beg c-end)
      (with-current-buffer buff
        (define-key file-map [mouse-1] 'calibredb-file-mouse-1)
        (define-key file-map [mouse-3] 'calibredb-file-mouse-3)
        (erase-buffer)
        (setq beg (point))
        ;; (insert (propertize (calibredb-show-metadata entry) 'calibredb-entry entry))
        (insert (format "ID          %s\n" (propertize id 'face 'calibredb-id-face)))
        (setq end (point))
        (put-text-property beg end 'calibredb-entry entry)
        (insert (format "Title       %s\n" (propertize title 'face 'calibredb-title-face)))
        (insert (format "Author_sort %s\n" (propertize author-sort 'face 'calibredb-author-face)))
        (insert (format "Tags        %s\n" (propertize tag 'face 'calibredb-tag-face)))
        (insert (format "Ids         %s\n" (propertize ids 'face 'calibredb-ids-face)))
        (insert (format "Date        %s\n" (propertize last_modified 'face 'calibredb-date-face)))
        (insert (format "Published   %s\n" (propertize pubdate 'face 'calibredb-pubdate-face)))
        (insert (format "Publisher   %s\n" (propertize publisher 'face 'calibredb-publisher-face)))
        (insert (format "Series      %s\n" (propertize series 'face 'calibredb-series-face)))
        (insert (format "Language    %s\n" (propertize lang_code 'face 'calibredb-language-face)))
        ;; (insert (format "File        %s\n" (propertize file 'face 'calibredb-file-face)))
        (insert (format "Format      %s\n" (mapconcat
                                            #'identity
                                            (-map (lambda (ext)
                                                    (propertize ext
                                                                'face 'calibredb-format-face
                                                                'mouse-face 'calibredb-mouse-face
                                                                'help-echo (if (s-contains? "http" file)
                                                                               file
                                                                             (expand-file-name
                                                                              (concat (file-name-base file) "." ext)
                                                                              (file-name-directory file)) )
                                                                'keymap file-map)) (s-split "," format)) ", ")))
        (insert (format "Size        %s\n" (propertize (concat size "Mb") 'face 'calibredb-size-face)))
        (cond ((equal calibredb-entry-render-comments "face")
               (insert (format "Comments    %s\n" (propertize comment 'face 'calibredb-comment-face))))
              ((equal calibredb-entry-render-comments "shr")
               (require 'shr)
               (insert "Comments\n")
               (setq c-beg (point))
               (insert comment)
               (setq c-end (point))
               (if (fboundp 'shr-render-region)
                   (shr-render-region c-beg c-end))
               (insert "\n"))
              ((equal calibredb-entry-render-comments "annotation")
               (insert "Comments\n\n")
               (require 'font-lock)
               (insert (calibredb-fontify comment 'calibredb-edit-annotation-mode))
               (insert "\n\n"))
              (t
               (insert (format "Comments    %s\n" (propertize comment 'face 'calibredb-comment-face)))))
        (insert "\n")
        (calibredb-insert-image cover "" calibredb-list-view-image-max-width calibredb-list-view-image-max-height)
        ;; (setq end (point))
        (calibredb-show-mode)
        (setq calibredb-show-entry entry)
        (goto-char (point-min))))
    (unless (eq major-mode 'calibredb-show-mode)
      (funcall calibredb-show-entry-switch buff)
      (when switch
        (switch-to-buffer-other-window (set-buffer (calibredb-search--buffer-name)))
        (goto-char original)))))

(defun calibredb-next-entry ()
  "Move to next entry."
  (interactive)
  (let ((ori "") (new ""))
    (while (and (equal new ori) new ori)
      (setq ori (calibredb-getattr (car (calibredb-find-candidate-at-point)) :id))
      (forward-line 1)
      (setq new (calibredb-getattr (car (calibredb-find-candidate-at-point)) :id)))))

(defun calibredb-previous-entry ()
  "Move to previous entry."
  (interactive)
  (let ((ori "") (new ""))
    (while (and (equal new ori) new ori (> (line-number-at-pos) 1))
      (forward-line -1)
      (save-excursion
        (setq ori (calibredb-getattr (car (calibredb-find-candidate-at-point)) :id))
        (forward-line -1)
        (setq new (calibredb-getattr (car (calibredb-find-candidate-at-point)) :id))))))

(defun calibredb-show-next-entry ()
  "Show next entry."
  (interactive)
  (calibredb-next-entry)
  (calibredb-show-entry (car (calibredb-find-candidate-at-point)) :switch))

(defun calibredb-show-previous-entry ()
  "Show previous entry."
  (interactive)
  (calibredb-previous-entry)
  (calibredb-show-entry (car (calibredb-find-candidate-at-point)) :switch))

(defun calibredb-search-buffer ()
  "Create buffer calibredb-search."
  (get-buffer-create "*calibredb-search*"))

(defun calibredb-search-header ()
  "TODO: Return the string to be used as the Calibredb header.
Indicating the library you use."
  (format "%s: %s   %s"
          (propertize calibredb-virtual-library-name 'face 'calibredb-search-header-library-name-face)
          (propertize calibredb-root-dir 'face 'calibredb-search-header-library-path-face)
          (concat
           (propertize (format "Total: %s"
                               (if (equal calibredb-search-entries '(""))
                                   "0   "
                                 (concat (number-to-string (length calibredb-search-entries)) "  "))) 'face 'calibredb-search-header-total-face)
           (cond ((eq calibredb-sort-by 'id)
                  "Sort: id ")
                 ((eq calibredb-sort-by 'title)
                  "Sort: title ")
                 ((eq calibredb-sort-by 'author)
                  "Sort: author ")
                 ((eq calibredb-sort-by 'format)
                  "Sort: format ")
                 ((eq calibredb-sort-by 'date)
                  "Sort: date ")
                 ((eq calibredb-sort-by 'pubdate)
                  "Sort: pubdate ")
                 ((eq calibredb-sort-by 'tag)
                  "Sort: tag ")
                 ((eq calibredb-sort-by 'size)
                  "Sort: size ")
                 ((eq calibredb-sort-by 'language)
                  "Sort: language ")
                 (t
                  "Sort: id "))
           (cond ((eq calibredb-order 'desc)
                  "↓  ")
                 ((eq calibredb-order 'asc)
                  "↑  ")
                 (t "↓  "))
           (propertize (format "%s%s"
                               (cond
                                (calibredb-tag-filter-p "Tag: ")
                                (calibredb-favorite-filter-p "Favorite: ")
                                (calibredb-author-filter-p "Author: ")
                                (calibredb-date-filter-p "Date: ")
                                (calibredb-format-filter-p "Format: ")
                                (t ""))
                               (if (equal calibredb-search-filter "")
                                   ""
                                 (concat calibredb-search-filter "   "))) 'face 'calibredb-search-header-sort-face)
           (propertize (let ((len (length (calibredb-find-marked-candidates))))
                         (if (> len 0)
                             (concat "Marked: " (number-to-string len)) "")) 'face 'calibredb-search-header-filter-face))))

(define-derived-mode calibredb-search-mode fundamental-mode "calibredb-search"
  "Major mode for listing calibre entries.
\\{calibredb-search-mode-map}"
  (setq truncate-lines t
        buffer-read-only t
        header-line-format '(:eval (funcall calibredb-search-header-function)))
  (buffer-disable-undo)
  (set (make-local-variable 'hl-line-face) 'calibredb-search-header-highlight-face)
  (hl-line-mode)
  (if (boundp 'ivy-sort-matches-functions-alist)
      (add-to-list 'ivy-sort-matches-functions-alist '(calibredb-add . ivy--sort-files-by-date)))
  (if (boundp 'ivy-alt-done-functions-alist)
      (add-to-list 'ivy-alt-done-functions-alist '(calibredb-add . ivy--directory-done)))
  (add-hook 'minibuffer-setup-hook #'calibredb-search--minibuffer-setup)
  (add-to-list 'mailcap-mime-extensions '(".epub" . "application/epub+zip"))
  (add-to-list 'mailcap-mime-extensions '(".mobi" . "application/x-mobipocket-ebook")))

(defun calibredb-search-mouse (event)
  "Visit the calibredb-entry click on.
Argument EVENT mouse event."
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (if (not (windowp window))
        (error "No ebook chosen"))
    (calibredb-show-entry (car (calibredb-find-candidate-at-point)))
    (select-window window)
    (set-buffer (calibredb-search--buffer-name))
    (goto-char pos)))

(defun calibredb-view ()
  "Visit the calibredb-entry."
  (interactive)
  (calibredb-show-entry (car (calibredb-find-candidate-at-point)) :switch))

(defun calibredb-search-refresh ()
  "Refresh calibredb."
  (interactive)
  (setq calibredb-search-entries (calibredb-candidates))
  (setq calibredb-full-entries calibredb-search-entries)
  (calibredb))

(defun calibredb-search-refresh-or-resume (&optional begin position)
  "Refresh calibredb or resume the BEGIN point and windows POSITION."
  (interactive)
  (let (beg pos)
    (setq beg (or begin (point)))
    (setq pos (or position (window-start)))
    (if (not (equal calibredb-search-filter ""))
        (progn
          (calibredb-search-refresh)
          (calibredb-search-update :force))
      (calibredb-search-refresh))
    (set-window-start (selected-window) pos)
    (goto-char beg)
    (hl-line-mode 1)))

(defun calibredb-search-toggle-view-refresh ()
  "TODO Refresh calibredb when toggle view goto the the same id ebook."
  (interactive)
  (let ((id (calibredb-read-metadatas "id")))
    (if (not (equal calibredb-search-filter ""))
        (progn
          (calibredb-search-refresh)
          (calibredb-search-update :force))
      (calibredb-search-refresh))
    (while (not (equal id (calibredb-read-metadatas "id")))
      (forward-line 1))
    (beginning-of-line)
    (recenter)))

(defun calibredb-search-refresh-and-clear-filter ()
  "Refresh calibredb and clear the fitler keyword."
  (interactive)
  (calibredb-search-refresh)
  (setq calibredb-tag-filter-p nil)
  (setq calibredb-favorite-filter-p nil)
  (setq calibredb-author-filter-p nil)
  (setq calibredb-date-filter-p nil)
  (setq calibredb-format-filter-p nil)
  (calibredb-search-keyword-filter ""))

(defun calibredb-search-clear-filter ()
  "Clear the fitler keyword."
  (interactive)
  (setq calibredb-tag-filter-p nil)
  (setq calibredb-favorite-filter-p nil)
  (setq calibredb-author-filter-p nil)
  (setq calibredb-date-filter-p nil)
  (setq calibredb-format-filter-p nil)
  (calibredb-search-keyword-filter ""))

(defun calibredb-search-quit ()
  "Quit *calibredb-entry* or *calibredb-search*."
  (interactive)
  (when (eq major-mode 'calibredb-search-mode)
    (cond ((get-buffer "*calibredb-entry*")
           (pop-to-buffer "*calibredb-entry*")
           (if (< (length (window-prev-buffers)) 2)
               (progn
                 (quit-window)
                 (kill-buffer "*calibredb-entry*"))
             (kill-buffer "*calibredb-entry*")))
          ((get-buffer "*calibredb-search*")
           (quit-window)
           (kill-buffer "*calibredb-search*")))))

(defun calibredb-mark-at-point ()
  "Mark the current line."
  (interactive)
  (remove-overlays (line-beginning-position) (line-end-position))
  (let* ((beg (line-beginning-position))
         (end (line-end-position))
         (inhibit-read-only t)
         (overlay (make-overlay beg end)))
    (overlay-put overlay 'face 'calibredb-mark-face)
    (put-text-property beg end 'calibredb-mark ?>)))

(defun calibredb-mark-and-forward ()
  "Mark the current line and forward."
  (interactive)
  (calibredb-mark-at-point)
  (calibredb-next-entry))

(defun calibredb-unmark-and-forward ()
  "Unmark the current line and forward."
  (interactive)
  (calibredb-unmark-at-point)
  (calibredb-next-entry))

(defun calibredb-unmark-and-backward ()
  "Unmark the current line and backward."
  (interactive)
  (calibredb-previous-entry)
  (calibredb-unmark-at-point))

(defun calibredb-unmark-at-point ()
  "Unmark the current line."
  (interactive)
  (let* ((beg (line-beginning-position))
         (end (line-end-position))
         (inhibit-read-only t))
    (remove-overlays (line-beginning-position) (line-end-position))
    (remove-text-properties beg end '(calibredb-mark nil))))

(defun calibredb-condense-comments (str)
  "Condense whitespace in STR into a single space."
  (replace-regexp-in-string "[[:space:]\n\r]+" " " str))

(defun calibredb-favorite-mouse-1 (event)
  "Visit the location click on.
Argument EVENT mouse event."
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (if (not (windowp window))
        (error "No favorite chosen"))
    (with-current-buffer (window-buffer window)
      (goto-char pos)
      (setq calibredb-tag-filter-p nil)
      (setq calibredb-favorite-filter-p t)
      (setq calibredb-author-filter-p nil)
      (setq calibredb-date-filter-p nil)
      (setq calibredb-format-filter-p nil)
      (calibredb-search-keyword-filter calibredb-favorite-keyword))))

(defun calibredb-tag-mouse-1 (event)
  "Visit the location click on.
Argument EVENT mouse event."
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (if (not (windowp window))
        (error "No tag chosen"))
    (with-current-buffer (window-buffer window)
      (goto-char pos)
      (setq calibredb-tag-filter-p t)
      (setq calibredb-favorite-filter-p nil)
      (setq calibredb-author-filter-p nil)
      (setq calibredb-date-filter-p nil)
      (setq calibredb-format-filter-p nil)
      (calibredb-search-keyword-filter (get-text-property (point) 'tag nil)))))

(defun calibredb-author-mouse-1 (event)
  "Visit the location click on.
Argument EVENT mouse event."
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (if (not (windowp window))
        (error "No author chosen"))
    (with-current-buffer (window-buffer window)
      (goto-char pos)
      (setq calibredb-tag-filter-p nil)
      (setq calibredb-favorite-filter-p nil)
      (setq calibredb-author-filter-p t)
      (setq calibredb-date-filter-p nil)
      (setq calibredb-format-filter-p nil)
      (calibredb-search-keyword-filter (get-text-property (point) 'author nil)))))

(defun calibredb-format-mouse-1 (event)
  "Visit the location click on.
Argument EVENT mouse event."
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (if (not (windowp window))
        (error "No format chosen"))
    (with-current-buffer (window-buffer window)
      (goto-char pos)
      (setq calibredb-tag-filter-p nil)
      (setq calibredb-favorite-filter-p nil)
      (setq calibredb-author-filter-p nil)
      (setq calibredb-date-filter-p nil)
      (setq calibredb-format-filter-p t)
      (calibredb-search-keyword-filter (word-at-point t)))))

(defun calibredb-date-mouse-1 (event)
  "Visit the location click on.
Argument EVENT mouse event."
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (if (not (windowp window))
        (error "No author chosen"))
    (with-current-buffer (window-buffer window)
      (goto-char pos)
      (setq calibredb-tag-filter-p nil)
      (setq calibredb-favorite-filter-p nil)
      (setq calibredb-author-filter-p nil)
      (setq calibredb-date-filter-p t)
      (setq calibredb-format-filter-p nil)
      (calibredb-search-keyword-filter (thing-at-point 'symbol t)))))

(defun calibredb-file-mouse-1 (event)
  "Visit the file click on.
Argument EVENT mouse event."
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (if (not (windowp window))
        (error "No ebook chosen"))
    (with-current-buffer (window-buffer window)
      (find-file-other-window (get-text-property pos 'help-echo nil)))))

(defun calibredb-file-mouse-3 (event)
  "Visit the file click on in default tool.
Argument EVENT mouse event."
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (if (not (windowp window))
        (error "No ebook chosen"))
    (with-current-buffer (window-buffer window)
      (calibredb-open-with-default-tool (get-text-property pos 'help-echo nil)))))

;; favorite

(defun calibredb-toggle-favorite-at-point (&optional keyword)
  "Toggle favorite the current item.
Argument KEYWORD is the tag keyword."
  (interactive)
  (let ((candidates (calibredb-find-marked-candidates)))
    (unless candidates
      (setq candidates (calibredb-find-candidate-at-point)))
    (calibredb-toggle-metadata-process candidates (or keyword calibredb-favorite-keyword ))))

;; highlight
(defun calibredb-toggle-highlight-at-point (&optional keyword)
  "Toggle highlight the current item.
Argument KEYWORD is the tag keyword."
  (interactive)
  (let ((candidates (calibredb-find-marked-candidates)))
    (unless candidates
      (setq candidates (calibredb-find-candidate-at-point)))
    (calibredb-toggle-metadata-process candidates (or keyword calibredb-highlight-keyword ))))

;; archive
(defun calibredb-toggle-archive-at-point (&optional keyword)
  "Toggle archive the current item.
Argument KEYWORD is the tag keyword."
  (interactive)
  (let ((candidates (calibredb-find-marked-candidates)))
    (unless candidates
      (setq candidates (calibredb-find-candidate-at-point)))
    (calibredb-toggle-metadata-process candidates (or keyword calibredb-archive-keyword ))))

(defun calibredb-toggle-metadata-process (cands keyword)
  "Run sequential processes to toggle metadata.
Argument CANDS is the list of candiates.
Argument KEYWORD is the metadata keyword to be toggled."
  (let* ((cand (pop cands))
         (tags (calibredb-read-metadatas "tags" cand)))
    ;; (pp cand)
    (if cand
        (set-process-sentinel
         (let* ((id (calibredb-getattr cand :id)))
           (if (s-contains? keyword tags)
               (calibredb-process :command "set_metadata"
                                  :option (format "--field tags:\"%s\"" (s-replace keyword "" tags))
                                  :id id
                                  :library (format "--library-path \"%s\"" calibredb-root-dir))
             (calibredb-process :command "set_metadata"
                                :option (format "--field tags:\"%s,%s\"" tags keyword)
                                :id id
                                :library (format "--library-path \"%s\"" calibredb-root-dir))))
         (lambda (p _e)
           (when (= 0 (process-exit-status p))
             (calibredb-toggle-metadata-process cands keyword))))
      ;; if no candidate left to be processed, refresh *calibredb-search*
      (cond ((equal major-mode 'calibredb-show-mode)
             (calibredb-show-refresh))
            ((eq major-mode 'calibredb-search-mode)
             (calibredb-search-refresh-or-resume))
            (t nil)))))
;; live filtering

(defun calibredb-search--update-list ()
  "Update `calibredb-search-entries' list."
  ;; replace space with _ (SQL) The underscore represents a single character
  (let* ((filter (calibredb-search-parse-filter calibredb-search-filter)) ;; (replace-regexp-in-string " " "_" calibredb-search-filter)
         (head (calibredb-candidate-filter filter)))
    ;; Determine the final list order
    (let ((entries head))
      (setf calibredb-search-entries
            entries))))

(defun calibredb-search-print-entry--default (entry)
  "Print ENTRY to the buffer."
  (unless (equal entry "")
    (let ((content (car entry)) beg end)
      (setq beg (point))
      (insert content)
      (calibredb-detailed-view-insert-image entry)
      (setq end (point))
      (put-text-property beg end 'calibredb-entry entry))))

(defun calibredb-search--minibuffer-setup ()
  "Set up the minibuffer for live filtering."
  (when calibredb-search-filter-active
    (when (eq :live calibredb-search-filter-active)
      (add-hook 'post-command-hook #'calibredb-search--live-update nil :local))))

(defun calibredb-search--live-update ()
  "Update the calibredb-search buffer based on the contents of the minibuffer."
  (when (eq :live calibredb-search-filter-active)
    ;; (message "HELLO")
    (let ((buffer (calibredb-search-buffer))
          (current-filter (minibuffer-contents-no-properties)))
      (when buffer
        (with-current-buffer buffer
          (let ((calibredb-search-filter current-filter))
            (calibredb-search-update :force)))))))

(defun calibredb-search-live-filter ()
  "Filter the calibredb-search buffer as the filter is written.
Currently, the filtering is column-oriented, not buffer oriented.
The following columns will be searched:

- id
- text
- tag
- title
- format
- author_sort

If the keyword occurs in any of the columns above, the matched
ebook record will be shown.

1. Live filter is faster than before since it search the results
   in =calibredb-full-entries= rather than query the database.

2. The keyword supports REGEX.

3. Inserting Spaces between
   keywords can narrow down the search results."

  (interactive)
  (unwind-protect
      (let ((calibredb-search-filter-active :live))
        (setq calibredb-search-filter
              (read-from-minibuffer (format "Filter %s: "
                                            (cond
                                             (calibredb-tag-filter-p "(tag)")
                                             (calibredb-favorite-filter-p "(favorite)")
                                             (calibredb-author-filter-p "(author)")
                                             (calibredb-date-filter-p "(date)")
                                             (calibredb-format-filter-p "(format)")
                                             (t "(live)"))) calibredb-search-filter))
        (message calibredb-search-filter))
    (calibredb-search-update :force)))

(defun calibredb-search-keyword-filter (keyword)
  "Filter the calibredb-search buffer with KEYWORD."
  (setq calibredb-search-filter keyword)
  (calibredb-search-update :force))

(defun calibredb-search-update (&optional force)
  "Update the calibredb-search buffer listing to match the database.
When FORCE is non-nil, redraw even when the database hasn't changed."
  (interactive)
  (with-current-buffer (calibredb-search-buffer)
    (when force
      (let ((inhibit-read-only t)
            (standard-output (current-buffer)))
        (erase-buffer)
        ;; reset calibredb-virtual-library-name
        (unless (-contains? (mapcar #'cdr calibredb-virtual-library-alist) calibredb-search-filter)
          (setq calibredb-virtual-library-name calibredb-virtual-library-default-name))
        (calibredb-search--update-list)
        ;; (setq calibredb-search-entries (calibredb-candidates))
        (dolist (entry calibredb-search-entries)
          (funcall calibredb-search-print-entry-function entry)
          (insert "\n"))
        ;; (insert "End of entries.\n")
        (goto-char (point-min))         ; back to point-min after filtering
        (setf calibredb-search-last-update (float-time))))))

(defun calibredb-search-parse-filter (filter)
  "Parse the elements of a search FILTER into a plist."
  (let ((matches ()))
    (cl-loop for element in (split-string filter) collect
             (when (calibredb-valid-regexp-p element)
               (push element matches)))
    `(,@(if matches
            (list :matches matches)))))

(defun calibredb-valid-regexp-p (regexp)
  "Return t if REGEXP is a valid REGEXP."
  (ignore-errors
    (prog1 t
      (string-match-p regexp ""))))

(defun calibredb-candidate-filter (filter)
  "Generate ebook candidate alist.
ARGUMENT FILTER is the filter string."
  (let ((matches (plist-get filter :matches))
        res-list)
    (cond (calibredb-tag-filter-p
           (cl-loop for line in calibredb-full-entries do
             (if (eval `(and ,@(cl-loop for regex in matches collect
                                        (unless (equal (calibredb-tag-width) 0) (s-contains? regex (calibredb-getattr (cdr line) :tag))))))
                 (push line res-list))))
          (calibredb-format-filter-p
           (cl-loop for line in calibredb-full-entries do
                    (if (eval `(and ,@(cl-loop for regex in matches collect
                                               (unless (equal (calibredb-format-width) 0) (s-contains? regex (calibredb-getattr (cdr line) :book-format))))))
                        (push line res-list))))
          (calibredb-author-filter-p
           (cl-loop for line in calibredb-full-entries do
                    (if (eval `(and ,@(cl-loop for regex in matches collect
                                               (unless (equal (calibredb-author-width) 0) (s-contains? regex (calibredb-getattr (cdr line) :author-sort))))))
                        (push line res-list))))
          (calibredb-date-filter-p
           (cl-loop for line in calibredb-full-entries do
                    (if (eval `(and ,@(cl-loop for regex in matches collect
                                               (unless (equal (calibredb-date-width) 0) (s-contains? regex (calibredb-getattr (cdr line) :last_modified))))))
                        (push line res-list))))
          (t (cl-loop for line in calibredb-full-entries do
             (if (eval `(and ,@(cl-loop for regex in matches collect
                                        (or
                                         (unless (equal calibredb-id-width 0) (string-match-p regex (calibredb-getattr (cdr line) :id)))
                                         (unless (equal (calibredb-title-width) 0) (string-match-p regex (calibredb-getattr (cdr line) :book-title)))
                                         (unless (equal (calibredb-format-width) 0) (string-match-p regex (calibredb-getattr (cdr line) :book-format)))
                                         (unless (equal (calibredb-tag-width) 0) (string-match-p regex (calibredb-getattr (cdr line) :tag)))
                                         (unless (equal (calibredb-ids-width) 0) (string-match-p regex (calibredb-getattr (cdr line) :ids)))
                                         (unless (equal (calibredb-author-width) 0) (string-match-p regex (calibredb-getattr (cdr line) :author-sort)))
                                         (unless (equal (calibredb-date-width) 0) (string-match-p regex (calibredb-getattr (cdr line) :last_modified)))
                                         ;; Normally, comments are long, it is necessary to trancate the comments to speed up the searching
                                         ;; except calibredb-comment-width is -1.
                                         (unless (equal (calibredb-comment-width) 0) (string-match-p regex (let ((c (calibredb-getattr (cdr line) :comment))
                                                                                                                 (w calibredb-comment-width))
                                                                                                             (if (> w 0) (s-truncate w c) c))))))))
                 (push line res-list)))))
    (nreverse res-list)))

;;; detailed view

(defun calibredb-toggle-view ()
  "Toggle between detailed view or compact view in *calibredb-search* buffer."
  (interactive)
  (setq calibredb-detailed-view (not calibredb-detailed-view))
  (calibredb-search-toggle-view-refresh))

(defun calibredb-detail-view-insert-image (entry)
  "Insert image in *calibredb-search* under detail view based on ENTRY."
  (if (and calibredb-detial-view calibredb-detailed-view-image-show)
      (let ((num (cond (calibredb-format-all-the-icons 3)
                       (calibredb-format-icons-in-terminal 3)
                       ((>= calibredb-id-width 0) calibredb-id-width)
                       (t 0 ))))
        (insert "\n")
        (insert (make-string num ? ))
        (calibredb-insert-image (calibredb-get-cover (cdr entry)) "" calibredb-detailed-view-image-max-width calibredb-detailed-view-image-max-height))))

(defun calibredb-detailed-view-insert-image (entry)
  "Insert image in *calibredb-search* under detailed view based on ENTRY."
  (if (and calibredb-detailed-view calibredb-detailed-view-image-show)
      (let* ((num (cond (calibredb-format-all-the-icons 3)
                        (calibredb-format-icons-in-terminal 3)
                        ((>= calibredb-id-width 0) calibredb-id-width)
                        (t 0 )))
             (file (calibredb-getattr (cdr entry) :file-path))
             (format (calibredb-getattr (cdr entry) :book-format))
             (cover (concat (file-name-directory file) "cover.jpg")))
          (if (image-type-available-p (intern format))
              (progn
                (insert "\n")
                (insert (make-string num ? ))
                (calibredb-insert-image file "" calibredb-detailed-view-image-max-width calibredb-detailed-view-image-max-height))
            (progn
              (insert "\n")
              (insert (make-string num ? ))
              (calibredb-insert-image cover "" calibredb-detailed-view-image-max-width calibredb-detailed-view-image-max-height))))))

(defun calibredb-toggle-view-at-point ()
  "Toggle between detailed view or compact view in *calibredb-search* buffer at point."
  (interactive)
  (let ((inhibit-read-only t)
        (status calibredb-detailed-view))
    (if calibredb-detailed-view
        ;; detailed view
        (cond
         ;; save to calibredb-entry
         ((get-text-property (point) 'calibredb-entry nil)
          (setq calibredb-detailed-view nil)
          (let* ((original (get-text-property (point) 'calibredb-entry nil))
                 (entry (cadr original))
                 (format (list (calibredb-format-item entry)))
                 ;; (position (seq-position calibredb-search-entries original))
                 (id (calibredb-get-init "id" (cdr (get-text-property (point) 'calibredb-entry nil)))) ; the "id" of current point
                 d-beg d-end)
            (if (equal id (calibredb-get-init "id" (cdr (get-text-property (point-min) 'calibredb-entry nil))))
                (setq d-beg (point-min))
              (save-excursion (while (equal id (calibredb-get-init "id" (cdr (get-text-property (point) 'calibredb-entry nil))))
                                (forward-line -1))
                              (forward-line 1)
                              (setq d-beg (point))))
            (save-excursion (while (equal id (calibredb-get-init "id" (cdr (get-text-property (point) 'calibredb-entry nil))))
                              (forward-line 1))
                            (goto-char (1- (point)))
                            (setq d-end (point)))
            (delete-region d-beg d-end)
            (save-excursion
              (unless (equal format "")
                (let ((content (car format))
                      (list (cons (car format) (list entry)))
                      beg end)
                  (setq beg (point))
                  (insert content)
                  (setq end (point))
                  (put-text-property beg end 'calibredb-compact list)))))
          (setq calibredb-detailed-view status))

         ;; save to calibredb-compact
         ((get-text-property (point) 'calibredb-compact nil)
          (setq calibredb-detailed-view t)
          (let* ((original (get-text-property (point) 'calibredb-compact nil))
                 (entry (cadr original))
                 (format (list (calibredb-format-item entry))))
            (delete-region (line-beginning-position) (line-end-position))
            (save-excursion
              (unless (equal format "")
                (let ((content (car format))
                      (list (cons (car format) (list entry)))
                      beg end)
                  (setq beg (point))
                  (insert content)
                  (calibredb-detailed-view-insert-image original)
                  (setq end (point))
                  (put-text-property beg end 'calibredb-entry list)))))
          (setq calibredb-detailed-view status)))

      ;; compact view
      (cond
       ;; save to calibredb-entry
       ((get-text-property (point) 'calibredb-entry nil)
        (setq calibredb-detailed-view t)
        (let* ((original (get-text-property (point) 'calibredb-entry nil))
               (entry (cadr original))
               (format (list (calibredb-format-item entry))))
          (delete-region (line-beginning-position) (line-end-position))
          (save-excursion
            (unless (equal format "")
              (let ((content (car format))
                    (list (cons (car format) (list entry)))
                    beg end)
                (setq beg (point))
                (insert content)
                (calibredb-detailed-view-insert-image original)
                (setq end (point))
                (put-text-property beg end 'calibredb-detailed list)))))
        (setq calibredb-detailed-view status))

       ;; save to calibredb-detailed
       ((get-text-property (point) 'calibredb-detailed nil)
        (setq calibredb-detailed-view nil)
        (let* ((original (get-text-property (point) 'calibredb-detailed nil))
               (entry (cadr original))
               (format (list (calibredb-format-item entry)))
               (id (calibredb-get-init "id" (cdr (get-text-property (point) 'calibredb-detailed nil)))) ; the "id" of current point
               d-beg d-end)
          (if (equal id (calibredb-get-init "id" (cdr (get-text-property (point-min) 'calibredb-detailed nil))))
              (setq d-beg (point-min))
            (save-excursion (while (equal id (calibredb-get-init "id" (cdr (get-text-property (point) 'calibredb-detailed nil))))
                              (forward-line -1))
                            (forward-line 1)
                            (setq d-beg (point))))
          (save-excursion (while (equal id (calibredb-get-init "id" (cdr (get-text-property (point) 'calibredb-detailed nil))))
                            (forward-line 1))
                          (goto-char (1- (point)))
                          (setq d-end (point)))
          (delete-region d-beg d-end)
          (save-excursion
            (unless (equal format "")
              (let ((content (car format))
                    (list (cons (car format) (list entry)))
                    beg end)
                (setq beg (point))
                (insert content)
                (setq end (point))
                (put-text-property beg end 'calibredb-entry list)))))
        (setq calibredb-detailed-view status))))))

(defun calibredb-fontify (string mode)
  "Fontify STRING with Major MODE."
  (with-temp-buffer
    (insert string)
    (delay-mode-hooks (funcall mode))
    (if (fboundp 'font-lock-ensure)
        (font-lock-ensure)
      (with-no-warnings
        (font-lock-fontify-buffer)))
    (buffer-string)))

(defun calibredb-copy-as-org-link ()
  "Copy the marked items as org links."
  (interactive)
  (let ((candidates (calibredb-find-marked-candidates)))
    (unless candidates
      (setq candidates (calibredb-find-candidate-at-point)))
    (kill-new
     (with-temp-buffer
       (dolist (cand candidates)
         (let ((id (calibredb-getattr cand :id))
               (path (calibredb-get-file-path cand t))
               (title (calibredb-getattr cand :book-title)))
           (insert (format "[[file:%s][%s %s - %s]]\n"
                           path
                           (cond (calibredb-format-all-the-icons
                                  (if (fboundp 'all-the-icons-icon-for-file)
                                      (all-the-icons-icon-for-file path) ""))
                                 (calibredb-format-icons-in-terminal
                                  (if (fboundp 'icons-in-terminal-icon-for-file)
                                      (icons-in-terminal-icon-for-file path :v-adjust 0 :height 1) ""))
                                 (t "")) id title))
           (message "Copied: %s - \"%s\" as org link." id title)))
       (buffer-string)))
    ;; remove overlays and text properties
    (let* ((beg (point-min))
           (end (point-max))
           (inhibit-read-only t))
      (remove-overlays beg end)
      (remove-text-properties beg end '(calibredb-mark nil)))))

(defmacro calibredb-sort-by (field)
  "Macro of functions calibredb-sort-by-*.
Argument FIELD."
  `(defun ,(intern (format "calibredb-sort-by-%s" field)) ()
     (interactive)
     ,(format "Sort by %s, refresh *calibredb-search*, and clear filter." field)
     (setq calibredb-sort-by (quote ,(intern field)))
     (calibredb-search-refresh-and-clear-filter)))

(calibredb-sort-by "id")
(calibredb-sort-by "title")
(calibredb-sort-by "format")
(calibredb-sort-by "author")
(calibredb-sort-by "date")
(calibredb-sort-by "pubdate")
(calibredb-sort-by "tag")
(calibredb-sort-by "size")
(calibredb-sort-by "language")

(defun calibredb-toggle-order ()
  "Toggle the order between descending or ascending."
  (interactive)
  (if (eq calibredb-order 'desc)
      (setq calibredb-order 'asc)
    (setq calibredb-order 'desc))
  (calibredb-search-refresh-and-clear-filter))


(provide 'calibredb-search)

;;; calibredb-search.el ends here
