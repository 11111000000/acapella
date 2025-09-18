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

(defun acapella--profile-url (profile)
  "Extract main URL for RPC from PROFILE."
  (or (cdr (assq 'url profile))
      (user-error "[Acapella] Profile missing :url")))

(defun acapella--mask-header (name value)
  "Return masked VALUE for header NAME for logging."
  (if (and (stringp name)
           (string-match-p (rx bos (or "authorization" "x-api-key") eos) (downcase name)))
      (concat (substring value 0 (min 8 (length value))) "…")
    value))

(provide 'acapella-config)

;;; acapella-config.el ends here
