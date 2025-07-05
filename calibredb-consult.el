;;; calibredb-consult.el --- Support consult for calibredb -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Damon Chan

;; Author: Damon Chan <elecming@gmail.com>
;; URL: https://github.com/chenyanming/calibredb.el
;; Keywords: tools
;; Version: 2.13.0

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
(require 'calibredb-utils)


(defcustom calibredb-consult-ripgrep-all-args
  "rga --null --line-buffered --color=never --max-columns=1000 --path-separator /\  --smart-case --no-heading --with-filename --line-number --type pdf"
  "Command line arguments for ripgrep, see `calibredb-consult-ripgrep-all'.
The dynamically computed arguments are appended.
Can be either a string, or a list of strings or expressions."
  :type '(choice string (repeat (choice string expression))))

(defun calibredb-consult-read (arg)
  "consult read for calibredb."
  (interactive "P")
  (if (fboundp 'consult--read)
      (let ((candidates (calibredb-candidates)))
        (if candidates
            (calibredb-find-file (consult--read candidates
                           :prompt "Pick a book: "
                           :lookup #'consult--lookup-cdr
                           :sort nil) arg)
          (message "INVALID LIBRARY")))))


(defun calibredb-consult--ripgrep-all-make-builder (paths)
  "Create ripgrep command line builder given PATHS."
  (let* ((cmd (consult--build-args calibredb-consult-ripgrep-all-args))
         (type (if (consult--grep-lookahead-p (car cmd) "-P") 'pcre 'extended)))
    (lambda (input)
      (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                   (flags (append cmd opts))
                   (ignore-case
                    (and (not (or (member "-s" flags) (member "--case-sensitive" flags)))
                         (or (member "-i" flags) (member "--ignore-case" flags)
                             (and (or (member "-S" flags) (member "--smart-case" flags))
                                  (let (case-fold-search)
                                    ;; Case insensitive if there are no uppercase letters
                                    (not (string-match-p "[[:upper:]]" arg))))))))
        (if (or (member "-F" flags) (member "--fixed-strings" flags))
            (cons (append cmd (list "-e" arg) opts paths)
                  (apply-partially #'consult--highlight-regexps
                                   (list (regexp-quote arg)) ignore-case))
          (pcase-let ((`(,re . ,hl) (funcall consult--regexp-compiler arg type ignore-case)))
            (when re
              (cons (append cmd (and (eq type 'pcre) '("-P"))
                            (list "-e" (consult--join-regexps re type))
                            opts paths)
                    hl))))))))

;;;###autoload
(defun calibredb-consult-ripgrep-all (&optional dir initial)
  "Search with `rga` for files in DIR where the content matches a regexp.
  The initial input is given by the INITIAL argument. See `consult-grep`
  for more details.
PS: Currently only support pdf, but it is still not perfect."
  (interactive "P")
  (if (fboundp 'consult--grep)
      (pcase-let* ((`(,prompt ,paths ,dir) (consult--directory-prompt "Search Calibredb: " (or dir calibredb-root-dir)))
                   (default-directory dir)
                   (builder (funcall #'calibredb-consult--ripgrep-all-make-builder paths))
                   (consult-preview-key nil))
        (consult--read
         (consult--async-command builder
           (consult--grep-format builder)
           :file-handler t) ;; allow tramp
         :prompt prompt
         :lookup (lambda (selected candidates &rest _)
                   (let* ((result selected)
                          (parts (split-string result ":"))
                          (file-name (car parts))
                          (page (when (string-match "Page \\([0-9]+\\)" (nth 2 parts))
                                  (match-string 1 (nth 2 parts)))))
                     (find-file file-name)
                     (when page
                       (cond
                        ((memq major-mode '(doc-view-mode pdf-view-mode))
                         (if (eq major-mode 'doc-view-mode)
                             (doc-view-goto-page (string-to-number page))
                           (pdf-view-goto-page (string-to-number page))))
                        (t ;; workaround, after find-file, the pdf is not yet opened, major-mode is still the old one
                         (require 'eaf)
                         (when-let* ((buffer (eaf-interleave--find-buffer (expand-file-name file-name))))
                           (switch-to-buffer buffer)
                           (eaf-interleave--display-buffer buffer)
                           (eaf-interleave--pdf-viewer-goto-page (expand-file-name file-name) (string-to-number page))))))))
         :state (consult--grep-state)
         :initial (consult--async-split-initial initial)
         :add-history (consult--async-split-thingatpt 'symbol)
         :require-match t
         :category 'consult-grep
         :group #'consult--prefix-group
         :history '(:input consult--grep-history)
         :sort nil))))

(provide 'calibredb-consult)

;;; calibredb-consult.el ends here
