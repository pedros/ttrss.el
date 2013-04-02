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

(require 'gnus)
(require 'nnoo)
(require 'nnheader)
(require 'url)
(require 'json)

(nnoo-declare nnttrss)

(defvoo nnttrss-address nil
  "Address of the tt-rss server.")

(defvoo nnttrss-user nil
  "Username to use for authentication to the tt-rss server.")

(defvoo nnttrss-password nil
  "Password to use for authentication to the tt-rss server.")

(defvar nnttrss-session-id nil
  "Current session id, if any, set after successful login.")

(defvar nnttrss-api-level nil
  "API version level, increased with each API functionality
  change.")

(defvar nnttrss-server-version nil
  "Server version number.")


;;; Session management

(defun nnttrss-post-request (address property &rest content)
  "Post urlencoded form data to ADDRESS. PROPERTY is keyword
  potentially in the response or nil. CONTENT must be a data
  structure that `json-encode' knows how to encode as a JSON
  object.

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
	(if (= status 0)
	    (or (plist-get content property) content)
	  (nnheader-report 'nnttrss (plist-get content :error)))))))

(defun nnttrss-login (address user password)
  "Login to the server at ADDRESS using USER and PASSWORD
credentials. Returns a session id string or nil."
  (nnttrss-post-request address
			:session_id
			:op "login"
			:user user
			:password password))

(defun nnttrss-logout (address session-id)
  "Logout of the server at ADDRESS using SESSION-ID credentials."
  (nnttrss-post-request address
			:status
			:op "logout"
			:sid session-id)

  (defun nnttrss-logged-in-p (address session-id)
    "Return t if there is a valid session at ADDRESS with
  SESSION-ID, false otherwise."
    (nnttrss-post-request address
			  :status
			  :op "isLoggedIn"
			  :sid session-id)))

(defun nnttrss-api-level (address session-id)
  "Return an integer corresponding to the API level at ADDRESS
  using SESSION-ID credentials."
  (nnttrss-post-request address
			:level
			:op "getApiLevel"
			:sid session-id))

(defun nnttrss-server-version (address session-id)
  "Return a string corresponding to the server version at ADDRESS
  using SESSION-ID credentials."
  (nnttrss-post-request address
			:version
			:op "getVersion"
			:sid session-id))


;;; Server statistics

(defun nnttrss-get-unread (address session-id)
  "Return number of unread artibles at ADDRESS using SESSION-ID
  credentials."
  (nnttrss-post-request nnttrss-address
			:unread
			:op "getUnread"
			:sid session-id))

(defun nnttrss-get-counters (address session-id)
  "Return a vector of plists corresponding to feeds, labels,
categories, or tags at ADDRESS using SESSION-ID credentials.
Each plist has the keywords :counter and :id and,
possibly, :has_img, :updated, and :kind."
  (nnttrss-post-request nnttrss-address
			nil
			:op "getCounters"
			:sid session-id))

(defun nnttrss-get-feeds (address session-id &rest params)
  "Return a vector of plists corresponding to feeds at ADDRESS
 using SESSION-ID credentials. PARAMS is any number of the
 following key-value pairs:

:cat_id integer
:unread_only boolean
:limit integer
:offset integer
:include_nested boolean

 Each plist has the keywords :last_updated, :cat_id, :order_id,
:feed_url, :unread, :title, :id, and :icon."
  (nnttrss-post-request nnttrss-address
			nil
			:op "getFeeds"
			:sid session-id
			params))	; FIXME: need to unwrap params
					; before handing them to
					; nnttrss-post-request




(gnus-declare-backend "nnttrss" 'address 'prompt-address)

(provide 'nnttrss)
;;; nnttrss.el ends here
