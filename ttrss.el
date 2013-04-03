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

(defun ttrss-get-config (address sid &optional property)
  "Return configuration plist of server at ADDRESS using SID.
Optionally, retrieve PROPERTY only, if it is one of the
following:

 :icons_dir          path to icons on the server filesystem
 :icons_url          path to icons when requesting them over http
 :daemon_is_running  whether update daemon is running
 :num_feeds          amount of subscribed feeds"
  (ttrss-post-request address
		      property
		      :op "getConfig"
		      :sid sid))

(defun ttrss-get-pref (address sid property)
  "Return value, at ADDRESS using SID, given by PROPERTY.

PROPERTY is one of the following fields:

 :allow_duplicate_posts        allow duplicate posts
 :auto_assign_labels           assign articles to labels automatically
 :blacklisted_tags             blacklisted tags
 :cdm_auto_catchup             automatically mark articles as read
 :cdm_expanded                 automatically expand articles in combined mode
 :combined_display_mode        combined feed display
 :confirm_feed_catchup         confirm marking feed as read
 :default_article_limit        amount of articles to display at once
 :default_update_interval      default interval between feed updates
 :digest_catchup               mark articles in e-mail digest as read
 :digest_enable                enable e-mail digest
 :digest_preferred_time        try to send digests around specified time
 :enable_api_access            enable external API
 :enable_feed_cats             enable feed categories
 :feeds_sort_by_unread         sort feeds by unread articles count
 :fresh_article_max_age        maximum age of fresh articles (in hours)
 :hide_read_feeds              hide feeds with no unread articles
 :hide_read_shows_special      show special feeds when hiding read feeds
 :long_date_format             long date format
 :on_catchup_show_next_feed    on catchup show next feed
 :purge_old_days               purge articles after this number of days
 :purge_unread_articles        purge unread articles
 :reverse_headlines            reverse headline order (oldest first)
 :short_date_format            short date format
 :show_content_preview         show content preview in headlines list
 :sort_headlines_by_feed_date  sort headlines by feed date
 :ssl_cert_serial              login with an SSL certificate
 :strip_images                 do not embed images in articles
 :strip_unsafe_tags            strip unsafe tags from articles
 :user_css_theme               select theme
 :user_stylesheet              customize stylesheet
 :user_timezone                user timezone
 :vfeed_group_by_feed          group headlines in virtual feeds"
  (ttrss-post-request address
		      :value
		      :op "getPref"
		      :sid sid
		      :pref_name property))

(defun ttrss-get-unread (address sid)
  "Return number of unread articles at ADDRESS using SID."
  (ttrss-post-request address
		      :unread
		      :op "getUnread"
		      :sid sid))


;;; Feed listings

(defun ttrss-get-counters (address sid)
  "Return vector of plists of counters at ADDRESS using SID.
Each plist has may represent a feed, category, label, or tag, and
has the following fields:

 :counter
 :id

And the following optional fields:

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

(defun ttrss-get-article (address sid &rest article-ids)
  "Return article plist vector from ADDRESS using SID with ARTICLE-IDS.

Returns nil or an empty vector if those ids can't be found.

Each plist has the following fields:

 :attachments
 :title
 :labels
 :published
 :link
 :content
 :id
 :marked
 :unread
 :comments
 :author
 :updated
 :feed_id

The value of :attachments is a vector of plists with the
following fields:

 :content_type
 :id
 :duration
 :content_url
 :post_id
 :title"
  (when article-ids
    (ttrss-post-request address
			nil
			:op "getArticle"
			:sid sid
			:article_id (mapconcat (lambda (i) (format "%d" i))
					       article-ids
					       ","))))


;;; Article manipulation

(defun ttrss-update-article (address sid &rest params)
  "Update articles at ADDRESS using SID based on PARAMS.

PARAMS is any number of the following key-value pairs:

 :article_ids  (repeat integer)  article IDs to operate on
 :mode         integer           0: false, 1: true, 2: toggle
 :field        integer           0: starred, 1: published, 2: unread, 3: article
 :data         string            optional data parameter when setting note field

Returns number of articles updated."
  (apply 'ttrss-post-request
	 address
	 nil
	 :op "updateArticle"
	 :sid sid
	 params))



;;; TODO: Implement following methods:
;; catchupFeed
;; shareToPublished
;; subscribeToFeed
;; unsubscribeFeed
;; getFeedTree

(provide 'ttrss)
;;; ttrss.el ends here
