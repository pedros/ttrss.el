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

(gnus-declare-backend "nnttrss" 'address 'prompt-address)

(provide 'nnttrss)
