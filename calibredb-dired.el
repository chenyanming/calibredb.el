;;; calibredb-dired.el --- Dired support for calibredb -*- lexical-binding: t; -*-

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
(declare-function calibredb-search-clear-filter "calibredb-search.el")

(define-obsolete-function-alias #'calibredb-open-dired
  'calibredb-dired-open "calibredb 2.12.0")

(defun calibredb-dired-open (&optional candidate arg)
  "Open dired of the selected item.
If the universal prefix ARG is used then open the folder
containing the current file by the default explorer.
Optional argument CANDIDATE is the selected item.
Opens a dired buffer in FILE's directory.  If FILE is a
directory, open this directory."
  (interactive
   (list (car (calibredb-find-candidate-at-point))
         current-prefix-arg))
  (if current-prefix-arg
      (calibredb-open-with-default-tool (file-name-directory (calibredb-get-file-path candidate t) ))
    (let ((file (calibredb-getattr candidate :file-path)))
      (if (file-directory-p file)
          (dired file)
        (dired (file-name-directory file))
        (dired-goto-file file)))))


(defun calibredb-dired-add ()
  "Add marked files in dired to current calibre library."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (calibredb-dired-add-process (dired-get-marked-files))))

(defun calibredb-dired-add-process (files)
  "The process of adding marked FILES in dired to current calibre
library."
  (let ((files (mapconcat
                (lambda (file)
                  (shell-quote-argument (expand-file-name file)))
                files " "))
        (buffer (current-buffer)))
    (set-process-sentinel
     (calibredb-process :command "add"
                        :input files
                        :library (if calibredb-add-duplicate
                                     (format "--library-path %s -d" (calibredb-root-dir-quote))
                                   (format "--library-path %s" (calibredb-root-dir-quote))))
     (lambda (p e)
       (when (= 0 (process-exit-status p))
         (setq calibredb-search-entries (calibredb-candidates))
         (setq calibredb-full-entries calibredb-search-entries)
         (calibredb-search-clear-filter)
         (with-current-buffer buffer
             (dired-do-delete)))))))

(provide 'calibredb-dired)

;;; calibredb-dired.el ends here
