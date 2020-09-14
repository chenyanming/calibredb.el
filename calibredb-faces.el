;;; calibredb-faces.el --- Faces for calibredb -*- lexical-binding: t; -*-

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

(defface calibredb-id-face
  '((((class color) (background light))
     :foreground "#3B6EA8")
    (((class color) (background dark))
     :foreground "#81A1C1")
    (t :inherit default))
  "Face used for id."
  :group 'calibredb-faces)

(defface calibredb-title-face '((t :inherit default))
  "Face used for title on compact view."
  :group 'calibredb-faces)

(defface calibredb-title-detail-view-face
  '((((class color) (background light))
     :background "gray85")
    (((class color) (background dark))
     :background "gray25")
    (t :inherit calibredb-title-face))
  "Face used for title on detail view."
  :group 'calibredb-faces)

(defface calibredb-author-face
  '((((class color) (background light))
     :foreground "#3B6EA8")
    (((class color) (background dark))
     :foreground "#d9c6d6")
    (t :inherit default))
  "Face used for author."
  :group 'calibredb-faces)

(defface calibredb-format-face
  '((((class color) (background light))
     :foreground "#4F894C")
    (((class color) (background dark))
     :foreground "#A3BE8C")
    (t :inherit default))
  "Face used for format."
  :group 'calibredb-faces)

(defface calibredb-comment-face
  '((((class color) (background light))
     :foreground "DarkCyan")
    (((class color) (background dark))
     :foreground "#8FBCBB")
    (t :inherit default))
  "Face used for comment."
  :group 'calibredb-faces)

(defface calibredb-tag-face
  '((((class color) (background light))
     :foreground "#8b94a5")
    (((class color) (background dark))
     :foreground "#EBCB8B")
    (t :inherit default))
  "Face used for tag."
  :group 'calibredb-faces)

(defface calibredb-ids-face
  '((((class color) (background light))
     :foreground "#8b94a5")
    (((class color) (background dark))
     :foreground "#EBCB8B")
    (t :inherit default))
  "Face used for ids."
  :group 'calibredb-faces)

(defface calibredb-size-face
  '((((class color) (background light))
     :foreground "#8b94a5")
    (((class color) (background dark))
     :foreground "#6f7787")
    (t :inherit default))
  "Face used for size."
  :group 'calibredb-faces)

(defface calibredb-pubdate-face '((t :inherit default))
  "Face for the publish date."
  :group 'calibredb-faces)

(defface calibredb-publisher-face '((t :inherit default))
  "Face for the publisher."
  :group 'calibredb-faces)

(defface calibredb-series-face '((t :inherit default))
  "Face for the series."
  :group 'calibredb-faces)

(defface calibredb-language-face '((t :inherit default))
  "Face for the language."
  :group 'calibredb-faces)

(defface calibredb-date-face
  '((((class color) (background light))
     :foreground "#29838D")
    (((class color) (background dark))
     :foreground "#8FBCBB")
    (t :inherit default))
  "Face for the date (last_modified)."
  :group 'calibredb-faces)

(defface calibredb-file-face
  '((((class color) (background light))
     :foreground "#29838D")
    (((class color) (background dark))
     :foreground "#8FBCBB")
    (t :inherit default))
  "Face for the file path."
  :group 'calibredb-faces)

(defface calibredb-mark-face '((t :inherit highlight))
  "Face for the mark candidate."
  :group 'calibredb-faces)

(defface calibredb-favorite-face
  '((((class color) (background light))
     :foreground "black")
    (((class color) (background dark))
     :foreground "yellow")
    (t :inherit default))
  "Face used for title."
  :group 'calibredb-faces)

(defface calibredb-highlight-face
  '((((class color) (background light))
     :foreground "MediumSlateBlue"
     :weight bold)
    (((class color) (background dark))
     :foreground "cyan"
     :weight bold)
    (t :inherit default))
  "Face used for hightlight."
  :group 'calibredb-faces)

(defface calibredb-archive-face
  '((((class color) (background light))
     :foreground "grey"
     :weight light)
    (((class color) (background dark))
     :foreground "dim grey"
     :weight light)
    (t :inherit default))
  "Face used for archive."
  :group 'calibredb-faces)

(defface calibredb-mouse-face '((t :inherit mode-line-highlight))
  "Face used for *calibredb-search* mouse face"
  :group 'calibredb-faces)

(defface calibredb-edit-annotation-header-title-face
  '((((class color) (background light))
     :foreground "#4F894C")
    (((class color) (background dark))
     :foreground "#A3BE8C")
    (t :inherit default))
  "Face used for *calibredb-edit-annotation* header tilte face"
  :group 'calibredb-faces)

(provide 'calibredb-faces)

;;; calibredb-faces.el ends here
