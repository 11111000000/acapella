;;; acapella-config.el --- Acapella configuration (A2A-first) -*- lexical-binding: t; -*-

;; Author: Acapella
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.2"))
;; Keywords: ai, agents, a2a
;; URL: https://example.com/acapella

;;; Commentary:
;; Minimal configuration and profile management for Acapella.
;; Keep functions small, simple, and pure where possible.

;;; Code:

(require 'auth-source)
(require 'subr-x)
(require 'url-parse)
(require 'acapella-util)

(defgroup acapella nil
  "Acapella — Emacs client for agent protocols (A2A-first)."
  :group 'applications)

(defcustom acapella-profiles
  ;; Minimal sample profile: A2A JSON-RPC endpoint (no auth).
  ;; If your agent card URL differs, you can still call JSON-RPC directly on :url.
  ;; Example A2A HelloWorld server runs at http://localhost:9999
  '(((name . "Local A2A HelloWorld")
     (protocol . a2a)
     (url . "http://localhost:9999/")      ;; main A2A JSON-RPC URL
     (headers . ())                         ;; extra headers
     (agent-card-url . "http://localhost:9999/.well-known/agent-card.json")
     (auth-source . nil)))                  ;; or an alist: ((machine . "host") (login . "user"))
  "List of Acapella server profiles (alist records).
Each profile is an alist with keys:
- name (string)            : logical profile name
- protocol ('a2a|'acp)     : protocol family (A2A prioritized)
- url (string)             : base URL for RPC
- headers (alist)          : extra HTTP headers, each as (NAME . VALUE)
- agent-card-url (string)  : optional Agent Card URL
- auth-source (alist|nil)  : optional auth-source query alist, e.g. ((machine . \"host\"))"
  :type '(repeat (alist :key-type symbol :value-type sexp))
  :group 'acapella)

(defcustom acapella-agent-card-ttl-seconds 300
  "TTL (in seconds) for cached Agent Cards fetched via HTTP.
If 0 or negative, caching is effectively disabled."
  :type 'integer
  :group 'acapella)

;; [moved] SSE reconnect settings are defined in acapella-transport.el:
;; - acapella-sse-auto-reconnect
;; - acapella-sse-reconnect-delay-seconds
;; - acapella-sse-reconnect-max

;; [moved] See acapella-transport.el defcustom `acapella-sse-reconnect-delay-seconds'.

;; [moved] See acapella-transport.el defcustom `acapella-sse-reconnect-max'.

(defvar acapella--current-profile-name nil
  "Name of the currently selected profile, or nil if none.")

(defun acapella-current-profile ()
  "Return the currently selected profile (alist), or nil."
  (when acapella--current-profile-name
    (seq-find (lambda (p)
                (string= (cdr (assq 'name p)) acapella--current-profile-name))
              acapella-profiles)))

(defun acapella-select-profile ()
  "Interactively select current Acapella profile."
  (interactive)
  (let* ((names (mapcar (lambda (p) (cdr (assq 'name p))) acapella-profiles))
         (choice (completing-read "Select Acapella profile: " names nil t)))
    (setq acapella--current-profile-name choice)
    (message "[Acapella] Profile selected: %s" choice)))

(defun acapella--headers-with-auth (profile)
  "Return headers alist for PROFILE, including optional Authorization from auth-source."
  (let* ((base (or (cdr (assq 'headers profile)) '()))
         (auth-q (cdr (assq 'auth-source profile)))
         (auth (when auth-q
                 (car (auth-source-search :max 1
                                          :host (cdr (assq 'machine auth-q))
                                          :user (cdr (assq 'login auth-q))
                                          :port (cdr (assq 'port auth-q))
                                          :require '(:secret)))))
         (token (when (and auth (functionp (plist-get auth :secret)))
                  (let (val) (setq val (funcall (plist-get auth :secret)))
                       (if (stringp val) val (and (listp val) (car val))))))
         (auth-h (when token (cons "Authorization" (format "Bearer %s" token)))))
    (if auth-h (cons auth-h base) base)))

(defun acapella--extensions-header (profile)
  "Return (\"X-A2A-Extensions\" . VALUE) from PROFILE's (extensions . LIST) if present."
  (let* ((ext (cdr (assq 'extensions profile))))
    (cond
     ((and (stringp ext) (not (string-empty-p ext)))
      (cons "X-A2A-Extensions" ext))
     ((and (listp ext) ext)
      (cons "X-A2A-Extensions" (mapconcat #'identity ext ", ")))
     (t nil))))

(defun acapella--headers-with-auth-and-ext (profile)
  "Return headers alist for PROFILE including Authorization and X-A2A-Extensions if configured."
  (let* ((base (acapella--headers-with-auth profile))
         (ext-h (acapella--extensions-header profile)))
    (if ext-h (cons ext-h base) base)))

(defun acapella--profile-url (profile)
  "Extract main URL for RPC from PROFILE."
  (or (cdr (assq 'url profile))
      (user-error "[Acapella] Profile missing :url")))

(defun acapella--mask-header (name value)
  "Return masked VALUE for header NAME for logging (delegates to util)."
  (acapella-util-mask-header name value))

(defcustom acapella-artifact-allowed-domains '("localhost" "127.0.0.1")
  "List of hostnames allowed for downloading artifacts from content_url.
Use nil to disallow all external downloads."
  :type '(repeat string)
  :group 'acapella)

(defcustom acapella-artifact-max-bytes 1048576
  "Maximum size (in bytes) allowed to download for a single artifact.
If the remote declares larger size or the response exceeds this, refuse to fetch."
  :type 'integer
  :group 'acapella)

(defcustom acapella-artifact-download-enabled nil
  "When non-nil, allow downloading artifacts from whitelisted domains.
MVP safeguard: disabled by default."
  :type 'boolean
  :group 'acapella)

(defcustom acapella-artifact-text-mime-prefixes '("text/" "application/json")
  "MIME type prefixes considered textual for artifact preview."
  :type '(repeat string)
  :group 'acapella)

(defun acapella--text-mime-p (content-type)
  "Return non-nil if CONTENT-TYPE should be treated as text for preview."
  (and (stringp content-type)
       (let ((ct (downcase content-type)))
         (seq-some (lambda (pfx) (string-prefix-p pfx ct))
                   acapella-artifact-text-mime-prefixes))))

(defun acapella-domain-allowed-p (url)
  "Return non-nil if URL hostname is in `acapella-artifact-allowed-domains'."
  (when (and (stringp url) (not (string-empty-p url)))
    (condition-case _
        (let* ((u (url-generic-parse-url url))
               (host (and u (url-host u))))
          (and host
               (member (downcase host)
                       (mapcar #'downcase acapella-artifact-allowed-domains))))
      (error nil))))

(provide 'acapella-config)

;;; acapella-config.el ends here
