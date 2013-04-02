;;; nnttrss.el --- interfacing with Tiny Tiny RSS

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


;;; Session management

(defun nnttrss-post-request (content address &optional property)
  "POST CONTENT to ADDRESS as urlencoded form data. CONTENT must
be a data structure that `json-encode' knows how to encode as a
JSON object.

Returns the JSON response as a plist or, optionally, the PROPERTY
in the plist if the response status is 0, nil otherwise."
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
	    (if (plist-member content property)
		(plist-get content property)
	      content)
	  (nnheader-report 'nnttrss (plist-get content :error)))))))

(defun nnttrss-login (address user password)
  "Login to the server at ADDRESS using USER and PASSWORD
credentials. Returns a session id string or nil."
  (nnttrss-post-request `(:op "login"
			      :user ,user
			      :password ,password)
			address
			:session_id))

(defun nnttrss-logout (address session-id)
  "Logout of the server at ADDRESS using SESSION-ID credentials."
  (nnttrss-post-request `(:op "logout"
			      :sid ,session-id)
			address)
  nil)

(defun nnttrss-logged-in-p (address session-id)
  "Return t if there is a valid session at ADDRESS with
  SESSION-ID, false otherwise."
  (nnttrss-post-request `(:op "isLoggedIn"
			      :sid ,session-id)
			address
			:status))

(defun nnttrss-api-level (address session-id)
  "Return an integer corresponding to the API level at ADDRESS
  using SID credentials."
  (nnttrss-post-request `(:op "getApiLevel"
			      :sid ,session-id)
			address
			:level))



(gnus-declare-backend "nnttrss" 'address 'prompt-address)

(provide 'nnttrss)
