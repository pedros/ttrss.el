;;; nnttrss.el --- interfacing with Tiny Tiny RSS

;; Copyright (C) 2013 Pedro Silva

;; Author: Pedro Silva <psilva+git@pedrosilva.pt>
;; Created: 01 April 2013
;; Version: 0.0.1
;; Keywords: news, local
;; Package-Requires ((emacs "23.1"))

;; This file is not part of GNU Emacs.

;; nnttrss is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; nnttrss is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with nnttrss.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ttrss)
(require 'gnus)
(require 'nnoo)
(require 'nnmail)
(require 'nnheader)
(require 'mm-util)

(nnoo-declare nnttrss)
(nnoo-define-basics nnttrss)
(gnus-declare-backend "nnttrss" 'address 'prompt-address)

(defvoo nnttrss-address nil
  "Address of the tt-rss server.")

(defvoo nnttrss-user nil
  "Username to use for authentication to the tt-rss server.")

(defvoo nnttrss-password nil
  "Password to use for authentication to the tt-rss server.")

(defvoo nnttrss-directory (nnheader-concat gnus-directory "ttrss/")
  "Where nnttrss will save its files.")

(defvoo nnttrss-status-string "")

(defvar nnttrss--sid nil
  "Current session id, if any, set after successful login.")

(defvar nnttrss--api-level nil
  "API version level, increased with each API functionality change.")

(defvar nnttrss--server-version nil
  "Server version number.")

(defvar nnttrss--headlines nil
  "List of all headline propertly lists.")

(defvar nnttrss--last-article-id nil
  "Internal server ID of last article nnttrss knows about.")

(defvar nnttrss--article-map nil
  "Property list of association lists.
The properties are group name strings.  Values are association
lists of SQL IDs to article numbers.")

(defvar nnttrss--feeds nil
  "List of all feed property lists.")

(deffoo nnttrss-open-server (server &optional defs)
  (if (nnttrss-server-opened server)
      t
    (let ((sid (ttrss-login nnttrss-address nnttrss-user nnttrss-password)))
      (setq nnttrss--sid sid
	    nnttrss--server-version (ttrss-get-version nnttrss-address nnttrss--sid)
	    nnttrss--api-level (ttrss-get-api-level nnttrss-address nnttrss--sid))))
  (nnoo-change-server 'nnttrss server defs))

(deffoo nnttrss-close-server (&optional server)
  (when (nnttrss-server-opened server)
    (ttrss-logout nnttrss-address nnttrss--sid)
    (setq nnttrss--sid nil
	  nnttrss--server-version nil
	  nnttrss--api-level nil)))

(deffoo nnttrss-server-opened (&optional server)
  (and nnttrss--sid (ttrss-logged-in-p nnttrss-address nnttrss--sid)))

(deffoo nnttrss-request-list (&optional server)
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (nnttrss--update-feeds)
    (nnttrss--write-feeds)
    (dolist (feed nnttrss--feeds)
      (let* ((title (plist-get feed :title))
	     (id (plist-get feed :id))
	     (article-ids (nnttrss--feed-articles id)))
	(if article-ids
	    (insert (format "\"%s\" %d %d y\n"
			    title
			    (apply 'max article-ids)
			    (apply 'min article-ids)))
	  (insert (format "\"%s\" 0 1 y\n" title)))))
    t))


(defun nnttrss--read-vars (&rest vars)
  "Read VARS from local file in 'nnttrss-directory'.
Sets the variables VARS'."
  (dolist (var vars)
    (setf (symbol-value var) nil)
    (let* ((name (symbol-name var))
	   (file (nnttrss-make-filename name))
	   (file-name-coding-system nnmail-pathname-coding-system))
      (when (file-exists-p file)
	(load file nil t t)))))

(defun nnttrss--write-vars (&rest vars)
  "Write VARS from memory to local file in 'nnttrss-directory'.
Assumes the variables VARS are set."
  (gnus-make-directory nnttrss-directory)
  (dolist (var vars)
    (let* ((name (symbol-name var))
	   (file (nnttrss-make-filename name))
	   (file-name-coding-system nnmail-pathname-coding-system)
	   (coding-system-for-write mm-universal-coding-system))
      (with-temp-file (nnttrss-make-filename name)
	(insert (format ";; -*- coding: %s; -*-\n"
			mm-universal-coding-system))
	(gnus-prin1 `(setq ,var ',(symbol-value var)))
	(insert "\n")))))

(defun nnttrss-make-filename (name)
  "Build filename based on NAME in 'nnttrss-directory'."
  (expand-file-name
   (nnrss-translate-file-chars
    (concat name ".el"))
   nnttrss-directory))

(defun nnttrss--feed-articles (feed-id)
  "Return list of article numbers corresponding to article IDs in FEED-ID."
  (let ((feed-article-map (plist-get nnttrss--article-map feed-id)))
    (mapcar (lambda (x) (cdr x)) feed-article-map)))

(defun nnttrss--read-feeds ()
  "Read feeds file in 'nnttrss-directory'.
Sets the variable 'nnttrss--feeds."
  (nnttrss--read-vars 'nnttrss--feeds))

(defun nnttrss--write-feeds ()
  "Write feeds from memory to local file in 'nnttrss-directory'.
Assumes the variable 'nnttrss--feeds' is set."
  (nnttrss--write-vars 'nnttrss-feeds))

(defun nnttrss--update-feeds ()
  "Update 'nnttrss--feeds'."
  (setq nnttrss--feeds (ttrss-get-feeds nnttrss-address
					nnttrss--sid
					:feed_id -4)))

(defun nnttrss--read-article-map ()
  "Read articles mapping file in 'nnttrss-directory'.
Sets the variable 'nnttrss--article-map."
  (nnttrss--read-vars 'nnttrss--article-map))

(defun nnttrss--write-article-map ()
  "Write article map from memory to local file in 'nnttrss-directory'.
Assumes the variable 'nnttrss--article-map' is set."
  (nnttrss--write-vars 'nnttrss--article-map))

(defun nnttrss--update-single-article-map (article-id group)
  "Add ARTICLE-ID in GROUP to 'nnttrss--article-map'."
  (if (not (plist-member nnttrss--article-map group))
      (setq nnttrss--article-map
	    (plist-put nnttrss--article-map group `((,article-id . 1))))
    (let ((mapping (plist-get nnttrss--article-map group)))
      (unless (assoc article-id mapping)
	(let* ((last-artno (cdar mapping))
	       (next-artno (+ 1 (or last-artno 0)))
	       (mapping (cons `(,article-id . ,next-artno) mapping)))
	  (setq nnttrss--article-map
		(plist-put nnttrss--article-map group mapping))))))  )

(defun nnttrss--update-article-map ()
  "Update 'nnttrss--article-map' with new articles in 'nnttrss--headlines'."
  (dolist (headline nnttrss--headlines)
    (let* ((article-id (plist-get headline :id))
	   (group (plist-get headline :feed_id)))
      (when (> article-id nnttrss--last-article-id)
	(nnttrss--update-single-article-map article-id group)))))

(defun nnttrss--get-article-number (article-id group)
  "Return article number corresponding to ARTICLE-ID in GROUP.
Note that ARTICLE-ID is an internal SQL identifier obtained from the API.
ARTICLE-NUMBER is the Gnus identifier."
  (cdr (assoc article-id (plist-get nnttrss--article-map group))))

(defun nnttrss--get-article-id (article-number group)
  "Return article id corresponding to ARTICLE-NUMBER in GROUP.
Note that ARTICLE-ID is an internal SQL identifier obtained from the API.
ARTICLE-NUMBER is the Gnus identifier."
  (car (rassoc article-number (plist-get nnttrss--article-map group))))

(defun nnttrss--read-headlines ()
  "Read headlines from local file in 'nnttrss-directory'.
Sets the variables 'nnttrss--headlines' and 'nnttrss--last-article-id'."
  (nnttrss--read-vars 'nnttrss--headlines 'nnttrss--last-article-id))

(defun nnttrss--write-headlines ()
  "Write headlines from memory to local file in 'nnttrss-directory'.
Assumes the variables 'nnttrss--headlines' and 'nnttrss--last-article-id' are set."
  (nnttrss--write-vars 'nnttrss--headlines 'nnttrss--last-article-id))

(defun nnttrss--update-headlines ()
  "Update 'nnttrss--headlines' since 'nnttrss--last-article-id'."
  (setq nnttrss--headlines (append nnttrss--headlines
				   (ttrss-get-headlines
				    nnttrss-address
				    nnttrss--sid
				    :feed_id -4
				    :limit -1
				    :since_id nnttrss--last-article-id)))
  (setq nnttrss--last-article-id (apply 'max (mapcar (lambda (x) (plist-get x :id))
						     nnttrss--headlines))))

(deffoo nnttrss-status-message (&optional server)
  nnttrss-status-string)

(provide 'nnttrss)
;;; nnttrss.el ends here
