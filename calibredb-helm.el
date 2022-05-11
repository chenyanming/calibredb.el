;;; calibredb-show.el --- Helm for calibredb -*- lexical-binding: t; -*-

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

(eval-when-compile (defvar calibredb-search-entries))
(eval-when-compile (defvar calibredb-full-entries))

(declare-function calibredb-set-metadata--tags "calibredb-utils.el")
(declare-function calibredb-set-metadata--comments "calibredb-utils.el")
(declare-function calibredb-open-file-with-default-tool "calibredb-utils.el")
(declare-function calibredb-show-entry "calibredb-search.el")
(declare-function calibredb-get-file-path "calibredb-utils.el")

(defvar calibredb-helm-map
  (if (boundp 'helm-map)
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map helm-map)
        (define-key map "\M-t" #'calibredb-set-metadata--tags-1)
        (define-key map "\M-c" #'calibredb-set-metadata--comments-1)
        map))
  "Keymap for `calibredb-find-helm'.")

(defcustom calibredb-helm-actions
  (if (fboundp 'helm-make-actions)
      (helm-make-actions
       "Open file"                   'calibredb-find-file
       "View details"                'calibredb-show-entry
       "Open file other frame"       'calibredb-find-file-other-frame
       "Open file with default tool" (lambda (candidate)
                                        (calibredb-open-file-with-default-tool nil candidate))
       "Open Cover Page"             'calibredb-find-cover
       "Set tags"          'calibredb-set-metadata--tags
       "Set comments"      'calibredb-set-metadata--comments
       "List fileds" 'calibredb-set-metadata--list-fields
       "Show metadata"               'calibredb-show-metadata
       "Export"                      'calibredb-export
       "Remove"                      'calibredb-remove
       "Insert an org link"          (lambda (candidate)
                                       (unless (featurep 'org)
                                         (require 'org))
                                       (if (fboundp 'org-insert-link)
                                           (org-insert-link nil (calibredb-get-file-path candidate t) (calibredb-getattr candidate :book-title))))
       "Mail Add attachment"         (lambda (candidate)
                                       (mail-add-attachment (calibredb-get-file-path candidate t)))))
  "Default actions for calibredb helm."
  :group 'calibredb
  :type '(alist :key-type string :value-type function))

(defun calibredb-helm-read ()
  "Helm read for calibredb."
  (when (fboundp 'helm)
    (when (get-buffer "*helm action*")
      (kill-buffer "*helm action*"))
    (unwind-protect
        (helm :sources (if (fboundp 'helm-build-sync-source)
                           (helm-build-sync-source "calibredb"
                             :header-name (lambda (name)
                                            (concat name " in [" calibredb-root-dir "]"))
                             :candidates (lambda ()
                                           (if calibredb-search-entries
                                               calibredb-search-entries
                                             (progn
                                               (setq calibredb-search-entries (calibredb-candidates))
                                               (setq calibredb-full-entries calibredb-search-entries))))
                             ;; :filtered-candidate-transformer 'helm-findutils-transformer
                             ;; :action-transformer 'helm-transform-file-load-el
                             :persistent-action 'calibredb-view--helm
                             :action 'calibredb-helm-actions
                             ;; :help-message 'helm-generic-file-help-message
                             :keymap calibredb-helm-map
                             :candidate-number-limit 9999
                             ;; :requires-pattern 3
                             ))
              :buffer "*helm calibredb*") )))

(defun calibredb-find-helm ()
  "Use helm to list all ebooks details."
  (interactive)
  (calibredb-helm-read))

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

(defun calibredb-view--helm (candidate)
  "Visit the calibredb-entry with helm.
Argument CANDIDATE is the selected candidate."
  (interactive)
  (calibredb-show-entry candidate))

(provide 'calibredb-helm)

;;; calibredb-helm.el ends here
