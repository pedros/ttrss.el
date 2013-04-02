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

(defvar nnttrss-server-version nil
  "Server version number.")


;;; Session management

(defun nnttrss-post-request (address &optional property &rest content)
  "Post urlencoded form data to ADDRESS. PROPERTY is keyword
  potentially in the response. CONTENT must be a data structure
  that `json-encode' knows how to encode as a JSON object.

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
			:sid session-id))

(defun nnttrss-logged-in-p (address session-id)
  "Return t if there is a valid session at ADDRESS with
  SESSION-ID, false otherwise."
  (nnttrss-post-request address
			:status
			:op "isLoggedIn"
			:sid session-id))

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



(gnus-declare-backend "nnttrss" 'address 'prompt-address)

(provide 'nnttrss)
;;; nnttrss.el ends here
