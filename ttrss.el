;;; ttrss.el --- Tiny Tiny RSS elisp bindings

;; Copyright (C) 2013 Pedro Silva

;; Author: Pedro Silva <psilva+git@pedrosilva.pt>
;; Created: 01 April 2013
;; Version: 0.0.1
;; Keywords: news, local
;; Package-Requires ((emacs "23.1"))

;; This file is not part of GNU Emacs.

;; ttrss is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ttrss is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ttrss.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-and-compile
  (autoload 'url-retrieve-synchronously "url")
  (autoload 'json-read "json")
  (autoload 'json-encode "json"))

(defvar ttrss-address nil
  "Address of the tt-rss server.")

(defvar ttrss-user nil
  "Username to use for authentication to the tt-rss server.")

(defvar ttrss-password nil
  "Password to use for authentication to the tt-rss server.")

(defvar ttrss-session-id nil
  "Current session id, if any, set after successful login.")

(defvar ttrss-api-level nil
  "API version level, increased with each API functionality change.")

(defvar ttrss-server-version nil
  "Server version number.")


;;; Utilities

(defun ttrss-post-request (address property &rest content)
  "Post to ADDRESS and possibly retrieve PROPERTY from the response to CONTENT.
CONTENT must be a data structure that `json-encode' knows how to
encode as a JSON object.

Returns the JSON response as a plist or, optionally, the PROPERTY
in the plist, if the response status is 0, nil otherwise."
  (let ((url-request-method "POST")
	(url-request-data (json-encode content))
	(json-object-type 'plist)
	(json-false nil))
    (with-current-buffer (url-retrieve-synchronously address)
      (goto-char (point-min))
      (search-forward-regexp "\n\n")
      (let* ((response (json-read))
	     (status (plist-get response :status))
	     (content (plist-get response :content)))
	(if (= status 1)
	    (user-error "API status error: %s"
			(mapconcat 'downcase
				   (split-string (plist-get content :error) "_")
				   " "))
	  (or (plist-get content property) content))))))


;;; Session management

(defun ttrss-login (address user password)
  "Login to the server at ADDRESS using USER and PASSWORD.
Returns a session id string or nil."
  (ttrss-post-request address
		      :session_id
		      :op "login"
		      :user user
		      :password password))

(defun ttrss-logout (address sid)
  "Logout of the server at ADDRESS using SID."
  (ttrss-post-request address
		      :status
		      :op "logout"
		      :sid sid))

(defun ttrss-logged-in-p (address sid)
  "Return t if there is a valid session at ADDRESS with SID."
  (ttrss-post-request address
		      :status
		      :op "isLoggedIn"
		      :sid sid))


;;; Server statistics

(defun ttrss-api-level (address sid)
  "Return the API level at ADDRESS using SID."
  (ttrss-post-request address
		      :level
		      :op "getApiLevel"
		      :sid sid))

(defun ttrss-server-version (address sid)
  "Return the server version at ADDRESS using SID."
  (ttrss-post-request address
		      :version
		      :op "getVersion"
		      :sid sid))


;;; Server statistics

(defun ttrss-get-unread (address session-id)
  "Return number of unread artibles at ADDRESS using SESSION-ID
credentials."
  (ttrss-post-request address
		      :unread
		      :op "getUnread"
		      :sid session-id))

 :has_img
 :updated
 :kind"
  (ttrss-post-request address
		      nil
		      :op "getCounters"
		      :sid sid))

(defun ttrss-get-feeds (address sid &rest params)
  "Return a vector of plists of feeds at ADDRESS using SID.
PARAMS is any number of the following key-value pairs:

 :cat_id          integer  return feeds under category cat_id
 :unread_only     boolean  only return feeds which have unread articles
 :limit           integer  limit amount of feeds returned to this value
 :offset          integer  skip this amount of feeds first
 :include_nested  boolean  include child categories

Special feed IDs are as follows:

 -1  starred
 -2  published
 -3  fresh
 -4  all articles
  0  archived
  IDs < -10 - labels

Each plist has the following fields:

 :last_updated
 :cat_id
 :order_id
 :feed_url
 :unread
 :title
 :id
 :icon."
  (apply 'ttrss-post-request
	 address
	 nil
	 :op "getFeeds"
	 :sid sid
	 params))

(defun ttrss-get-categories (address sid &rest params)
  "Return vector of category plists at ADDRESS using SID.
PARAMS is any number of the following key-value pairs:

 :unread_only    boolean  only return categories which have unread articles
 :enable_nested  boolean  switch to nested mode, only returns topmost categories
 :include_empty  boolean  include empty categories"
  (apply 'ttrss-post-request
	 address
	 nil
	 :op "getCategories"
	 :sid sid
	 params))

(defun ttrss-get-labels (address sid)
  "Return a vector of label plists at ADDRESS using SID.

Each plist has the following fields:

 :id
 :caption
 :fg_color
 :bg_color
 :checked"
  (ttrss-post-request address
		      nil
		      :op "getLabels"
		      :sid sid))


;;; Feed manipulation

(defun ttrss-update-feed (address sid feed-id)
  "Update the feed at ADDRESS using SID with FEED-ID.
This operation is not performed in the background, so it might
take considerable time and, potentially, be aborted by the HTTP
server."
  (ttrss-post-request address
		      :status
		      :op "updateFeed"
		      :sid sid
		      :feed_id feed-id))

(defun ttrss-set-article-label
  (address sid article-ids label-id &optional assign)
  "Update items at ADDRESS using SID given by ARTICLE-IDS with LABEL-ID.
Assign labels if ASSIGN is t, remove otherwise.

Returns number of articles updated."
  (ttrss-post-request address
		      :updated
		      :op "setArticleLabel"
		      :sid sid
		      :article-ids (mapconcat (lambda (i) (format "%d" i))
					     article-ids
					     ",")
		      :label_id label-id
		      :assign assign))


;;; Article listings

(defun ttrss-get-headlines (address sid &rest params)
  "Return a vector of headline plists at ADDRESS using SID.
PARAMS is any number of the following key-value pairs:

 :feed_id              integer  only output articles for this feed
 :limit                integer  limits the amount of returned articles
 :skip                 integer  skip this amount of feeds first
 :filter               string   currently unused
 :is_cat               boolean  requested feed_id is a category
 :show_excerpt         boolean  include article excerpt in the output
 :show_content         boolean  include full article text in the output
 :view_mode            string   all_articles, unread, adaptive, marked, updated
 :include_attachments  boolean  include article attachments
 :since_id             integer  articles with id greater than since_id
 :include_nested       boolean  include articles from child categories
 :search               string   search query
 :search_mode          string
 :match_on             string

Special feed IDs are as follows:

 -1  starred
 -2  published
 -3  fresh
 -4  all articles
  0  archived
 IDs < -10 - labels"
  (apply 'ttrss-post-request
	 address
	 nil
	 :op "getHeadlines"
	 :sid sid
	 params))



(provide 'ttrss)
;;; ttrss.el ends here
