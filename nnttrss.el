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



(defun nnttrss-post-request (content address)
  "POST CONTENT to ADDRESS as urlencoded form data. CONTENT must
be a data structure that `json-encode' knows how to encode as a
JSON object. Returns the JSON response as a plist or nil."
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
	(if (= status 0) content
	  (nnheader-report 'nnttrss (plist-get content :error)))))))

(defun nnttrss-login (address user password)
  "Login to the server at ADDRESS using USER and PASSWORD
credentials. Returns a session id string or nil."
  (let ((response (nnttrss-post-request `(:op "login"
					      :user ,user
					      :password ,password)
					address)))
    (plist-get response :session_id)))

(defun nnttrss-logout (address session-id)
  "Logout of the server at ADDRESS using SESSION-ID credentials."
  (nnttrss-post-request `(:op "logout"
			      :sid ,session-id)
			address))

(defun nnttrss-logged-in-p (address session-id)
  "Return t if there is a valid session at ADDRESS with
  SESSION-ID, false otherwise."
  (let ((response (nnttrss-post-request `(:op "isLoggedIn"
				      :sid ,session-id)
					address)))
    (plist-get response :status)))



(gnus-declare-backend "nnttrss" 'address 'prompt-address)

(provide 'nnttrss)
