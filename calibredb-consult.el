;;; calibredb-consult.el --- Support consult for calibredb -*- lexical-binding: t; -*-

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
(require 'calibredb-utils)

(eval-when-compile (defvar calibredb-search-entries))
(eval-when-compile (defvar calibredb-full-entries))

(defun calibredb-consult-read (arg)
  "consult read for calibredb."
  (interactive "P")
  (if (fboundp 'consult--read)
      (let ((candidates (if calibredb-search-entries
                      calibredb-search-entries
                    (progn
                      (setq calibredb-search-entries (calibredb-candidates))
                      (setq calibredb-full-entries calibredb-search-entries)))))
        (if candidates
            (calibredb-find-file (consult--read candidates
                           :prompt "Pick a book: "
                           :lookup #'consult--lookup-cdr
                           :sort nil) arg)
          (message "INVALID LIBRARY")))))

(provide 'calibredb-consult)

;;; calibredb-consult.el ends here
