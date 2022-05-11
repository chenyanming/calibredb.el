;;; calibredb-opds.el --- opds for calibredb -*- lexical-binding: t; -*-

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
(require 'esxml-query)
(require 'esxml)
(require 'dom)

(defvar calibredb-opds-root-url nil)
(defvar calibredb-opds-download-dir "~/Downloads")

(declare-function calibredb "calibredb.el")

(defun calibredb-opds-mailcap-mime-to-extn (mime)
  "Return the file extensions EXTN based on the MIME content type."
  (mailcap-parse-mimetypes)
  (if (stringp mime)
      (car (rassoc (downcase mime) mailcap-mime-extensions))))

(defun calibredb-opds-host ()
  "Modify `url-recreate-url' to fit the needs."
  (let* ((urlobj (url-generic-parse-url calibredb-opds-root-url))
         (type (url-type urlobj))
         (user (url-user urlobj))
         (pass (url-password urlobj))
         (host (url-host urlobj))
         ;; RFC 3986: "omit the port component and its : delimiter if
         ;; port is empty or if its value would be the same as that of
         ;; the scheme's default."
         (port (url-port-if-non-default urlobj))
         ;; (file (url-filename urlobj))
         ;; (frag (url-target urlobj))
         )
    (concat (if type (concat type ":"))
            (if (url-fullness urlobj) "//")
            (if (or user pass)
                (concat user
                        (if pass (concat ":" pass))
                        "@"))
            host
            (if port (format ":%d" (url-port urlobj)))
            ;; (or file "/")
            ;; (if frag (concat "#" frag))
            )))

(defun calibredb-opds-request-page (url &optional account password)
  "Request URL.
Optional argument ACCOUNT.
Optional argument PASSWORD."
  (require 'request)
  (message "Loading %s..." url)
  (let (output)
    (setq calibredb-opds-root-url url)
    (if (fboundp 'request)
        (request url
                 :parser 'buffer-string
                 :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36")
                            ("Content-Type" . "application/xml")
                            ,(if (and account password)
                                 `("Authorization" . ,(concat "Basic "
                                                              (base64-encode-string
                                                               (concat account ":" password))))))
                 :sync nil
                 :success (cl-function
                           (lambda (&key data &allow-other-keys)
                             (let* ((dom (with-temp-buffer
                                           (insert data)
                                           (libxml-parse-xml-region (point-min) (point-max)))))
                               (setq calibredb-search-entries (calibredb-opds-dom dom))
                               (setq calibredb-full-entries calibredb-search-entries)
                               (calibredb)
                               (setq calibredb-tag-filter-p nil)
                               (setq calibredb-favorite-filter-p nil)
                               (setq calibredb-author-filter-p nil)
                               (setq calibredb-date-filter-p nil)
                               (setq calibredb-format-filter-p nil)
                               (calibredb-search-keyword-filter "")
                               (message "")
                               ;; (setq output (opds-page dom))
                               )))))
    output))

(defun calibredb-opds-download (title url format &optional account password)
  "Download file of TITLE URL FORMAT.
Optional argument ACCOUNT.
Optional argument PASSWORD."
  (let* ((file (expand-file-name (format "%s%s" title format) calibredb-opds-download-dir))
         (cmd (if (and account password)
                  (format "curl -u %s:\"%s\" -L %s -o %s" account password (shell-quote-argument url) (shell-quote-argument file ))
                (format "curl -L %s -o %s" (shell-quote-argument url) (shell-quote-argument file)))))
    (message cmd)
    (if (file-exists-p file)
        (find-file file)
      (set-process-sentinel
       (start-process-shell-command "calibredb-opds" "*calibredb-opds*" cmd)
       (lambda (p _e)
         (when (= 0 (process-exit-status p))
           (if (file-exists-p file)
               (find-file file))))))))

(defun calibredb-opds-dom (dom)
  "Parse DOM."
  (let ((entries (-concat
                  (-map (lambda (link)
                          `(entry nil
                                  (title nil ,(esxml-node-attribute 'rel link))
                                  ,link))
                        (esxml-query-all "feed>link" dom))
                  (esxml-query-all "feed>entry" dom))))
    (nreverse (calibredb-getbooklist
     (let ((no 0))
       (-mapcat
        (lambda (entry)
          (setq no (1+ no))
          (list `(
                  ;; (:id                     ,(dom-text (esxml-query "id" entry)))
                  (:id                     ,(number-to-string no))
                  (:author-sort            ,(dom-text (esxml-query "author>name" entry))) ; TODO: support mutitple authors
                  (:book-dir               "")
                  (:book-cover             ,(let ((url (or (dom-attr (esxml-query "[type^=image]" entry) 'href) "")))
                                              (if (and (stringp url) (s-contains? "http" url))
                                                  url
                                                (cond ((s-equals-p "" url) nil) ; no image url, return nil
                                                      ((s-contains? "base64" url) url) ; base64 image
                                                      (t (format "%s%s" (calibredb-opds-host) url))))))
                  (:book-name              "")
                  (:book-format            ,(or (dom-attr (esxml-query "[type^=application]" entry) 'type) "")) ; TODO: support more formats
                  (:book-pubdate           ,(dom-text (or (esxml-query "issued" entry)
                                                          (esxml-query "published" entry))))
                  (:book-title             ,(dom-text (esxml-query "title" entry)))
                  (:file-path              ,(let ((url (or (dom-attr (esxml-query "[type^=application]" entry) 'href) "")))
                                              (if (and (stringp url) (s-contains? "http" url))
                                                  url
                                                (cond ((s-equals-p "" url) "")
                                                      ((s-equals-p (s-left 1 url) "/") (format "%s%s" (calibredb-opds-host) url))
                                                      (t (format "%s/%s" (calibredb-opds-host) url))))))
                  (:tag                    ,(mapconcat #'identity
                                                       (-map (lambda (cat)
                                                               (or (esxml-node-attribute 'label cat)
                                                                   (esxml-node-attribute 'term cat))) (esxml-query-all "category" entry)) ", "))
                  (:size                   ,(format "%.2f" (/ (string-to-number (or (dom-attr (esxml-query "[type^=application]" entry) 'length ) "0" ) ) 1048576.0) ))
                  (:comment                ,(cond ((esxml-query "summary" entry)
                                                   (dom-text (esxml-query "summary" entry)))
                                                  ((esxml-query "content" entry)
                                                   (esxml-to-xml (esxml-query "content" entry)))
                                                  (t "" )))
                  (:ids                    ,(dom-text (or (esxml-query "identifier" entry)
                                                          (esxml-query "id" entry))))
                  (:publisher              ,(dom-text (or (esxml-query "publisher>name" entry)
                                                          (esxml-query "publisher" entry))))
                  (:series                 "")
                  (:lang_code              ,(dom-text (esxml-query "language" entry)))
                  (:last_modified          ,(dom-text (esxml-query "updated" entry))))) )
        entries))) )))

(defun calibredb-opds-search (&optional url)
  "Search library from URL."
  (interactive)
  (let* ((url (or url (completing-read "Search library: " calibredb-library-alist)))
         (library (-first (lambda (lib)
                            (s-contains? (file-name-directory (car lib)) url))
                          calibredb-library-alist)))
    (calibredb-opds-request-page (format "%s/search\?query=%s" url  (read-string "Search: ")) (nth 1 library) (nth 2 library))))

(provide 'calibredb-opds)

;;; calibredb-opds.el ends here
