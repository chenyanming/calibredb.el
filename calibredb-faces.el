;;; calibredb/calibredb-faces.el -*- lexical-binding: t; -*-

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

(defface calibredb-search-header-highlight-face
  '((t :inherit region :weight bold :underline t))
  "Face for the header at point."
  :group 'calibredb-faces)

(defface calibredb-id-face '((t :inherit font-lock-keyword-face))
  "Face used for id."
  :group 'calibredb-faces)

(defface calibredb-title-face '((t :inherit default))
  "Face used for title on list view."
  :group 'calibredb-faces)

(defface calibredb-title-detail-view-face
  '((((class color) (background light))
     :background "gray85")
    (((class color) (background dark))
     :background "gray25")
    (t :inherit calibredb-title-face))
  "Face used for title on detail view."
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

(defface calibredb-pubdate-face '((t :inherit default))
  "Face for the publish date."
  :group 'calibredb-faces)

(defface calibredb-file-face '((t :inherit font-lock-function-name-face))
  "Face for the file path."
  :group 'calibredb-faces)

(defface calibredb-mark-face '((t :inherit highlight))
  "Face for the mark candidate."
  :group 'calibredb-faces)

(defface calibredb-favorite-face '((t :inherit default :foreground "yellow"))
  "Face used for title."
  :group 'calibredb-faces)

(defface calibredb-highlight-face '((t :inherit default :foreground "cyan"))
  "Face used for hightlight."
  :group 'calibredb-faces)

(defface calibredb-archive-face '((t :inherit default :foreground "dim grey"))
  "Face used for archive."
  :group 'calibredb-faces)

(defface calibredb-mouse-face '((t :inherit mode-line-highlight))
  "Face used for *calibredb-search* mouse face"
  :group 'calibredb-faces)

(defface calibredb-edit-annotation-header-title-face '((t :inherit font-lock-string-face))
  "Face used for *calibredb-edit-annotation* header tilte face"
  :group 'calibredb-faces)

(provide 'calibredb-faces)

;;; calibredb-faces.el ends here
