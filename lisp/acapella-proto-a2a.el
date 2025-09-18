;;; acapella-proto-a2a.el --- A2A adapter (JSON-RPC + SSE) -*- lexical-binding: t; -*-

;; Author: Acapella
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.2"))
;; Keywords: a2a, json-rpc
;; URL: https://example.com/acapella

;;; Commentary:
;; Minimal A2A adapter providing message/send and message/stream.
;; It builds JSON-RPC 2.0 envelopes and uses acapella-transport for IO.

;;; Code:

(require 'json)
(require 'subr-x)
(require 'acapella-util)
(require 'acapella-config)
(require 'acapella-transport)

(defun acapella-a2a--uuid ()
  "Return a simple unique string id."
  (secure-hash 'sha1 (format "%s-%s-%s" (float-time) (random) (user-uid))))

(defun acapella-a2a--json (obj)
  "Serialize OBJ (alist) to JSON string."
  (json-encode obj))

(defun acapella-a2a--parse-json (s)
  "Parse JSON string S to alist."
  (json-parse-string s :object-type 'alist :array-type 'list
                     :null-object json-null :false-object json-false))

(defun acapella-a2a--rpc-envelope (method params)
  "Build JSON-RPC 2.0 envelope for METHOD and PARAMS (alist)."
  `(("jsonrpc" . "2.0")
    ("id" . ,(acapella-a2a--uuid))
    ("method" . ,method)
    ,@(when params `(("params" . ,params)))))

(defun acapella-a2a--text-message (text &optional task-id context-id)
  "Create minimal A2A user Message alist from TEXT, optionally with TASK-ID, CONTEXT-ID."
  `(("role" . "user")
    ("parts" . ((( "kind" . "text") ("text" . ,(or text "")))))
    ,@(when task-id `(("taskId" . ,task-id)))
    ,@(when context-id `(("contextId" . ,context-id)))))

(defun acapella-a2a--http->jsonrpc (resp on-success)
  "Normalize HTTP RESP to JSON-RPC-like object. If 2xx, parse and call ON-SUCCESS with alist.
On error, return JSON-RPC-shaped error alist including WWW-Authenticate when present.
If Content-Type is present and not application/json, return a parse-like error."
  (let ((status (plist-get resp :status))
        (body   (plist-get resp :body))
        (hdrs   (plist-get resp :headers))
        (ctype  (plist-get resp :content-type)))
    (if (and status (>= status 200) (< status 300))
        (if (and (stringp ctype)
                 (not (string-match-p "application/json" (downcase ctype))))
            `(("jsonrpc" . "2.0")
              ("error" . (("code" . -32700)
                          ("message" . ,(format "Unexpected Content-Type: %s (expected application/json)" ctype)))))
          (condition-case err
              (funcall on-success (acapella-a2a--parse-json body))
            (error `(("jsonrpc" . "2.0")
                     ("error" . (("code" . -32700)
                                 ("message" . ,(error-message-string err))))))))
      (let* ((wa (cdr (acapella-util-ci-header-find hdrs "WWW-Authenticate"))))
        `(("jsonrpc" . "2.0")
          ("error" . (("code" . ,(- (or status 0)))
                      ("message" . ,(format "HTTP error %s" status))
                      ,@(when wa `(("data" . (("www-authenticate" . ,wa))))))))))))


;; JSON-RPC error normalization ------------------------------------------------

(defun acapella-a2a--normalize-jsonrpc-error (err)
  "Return normalized JSON-RPC ERR alist with friendlier message for known codes."
  (let* ((code (alist-get "code" err nil nil #'string=))
         (msg  (alist-get "message" err nil nil #'string=))
         (friendly
          (cond
           ((eq code -32700) "Malformed JSON received from server")
           ((eq code -32600) "Invalid JSON-RPC request")
           ((eq code -32601) "Method not found on server")
           ((eq code -32602) "Invalid params")
           ((eq code -32603) "Internal error on server")
           ;; A2A typical domain codes (-32000..-32099)
           ((eq code -32001) "Invalid input (A2A)")
           ((eq code -32002) "Unauthorized (A2A)")
           ((eq code -32003) "Forbidden (A2A)")
           ((eq code -32004) "Not Found (A2A)")
           ((eq code -32005) "Conflict (A2A)")
           ((eq code -32006) "Rate limited (A2A)")
           ((eq code -32007) "Service unavailable (A2A)")
           ;; HTTP mapped as negative status
           ((and (numberp code) (< code 0))
            (format "HTTP error %d" (- code)))
           (t nil))))
    (if friendly
        (let ((out (copy-sequence err)))
          (setf (alist-get "message" out nil nil #'string=)
                (if (and msg (not (string-empty-p msg)))
                    (format "%s: %s" friendly msg)
                  friendly))
          out)
      err)))

(defun acapella-a2a--normalize-jsonrpc (obj)
  "Normalize JSON-RPC OBJ alist: map error codes to friendly messages."
  (let ((err (alist-get "error" obj nil nil #'string=)))
    (if err
        (let ((norm (acapella-a2a--normalize-jsonrpc-error err)))
          (let ((out (copy-sequence obj)))
            (setf (alist-get "error" out nil nil #'string=) norm)
            out))
      obj)))

;; Generic small helpers for JSON-RPC calls and SSE streams --------------------

(defun acapella-a2a--json-headers (profile)
  "Return JSON headers for PROFILE: Content-Type + auth + extensions."
  (append '(("Content-Type" . "application/json"))
          (acapella--headers-with-auth-and-ext profile)))

(defun acapella-a2a--sse-headers (profile)
  "Return SSE headers for PROFILE: Accept + Content-Type + auth + extensions."
  (append '(("Accept" . "text/event-stream")
            ("Content-Type" . "application/json"))
          (acapella--headers-with-auth-and-ext profile)))

(defun acapella-a2a--rpc-call (profile method params on-result)
  "Small orchestrator: resolve URL, build envelope and call HTTP POST.
Calls ON-RESULT with normalized JSON-RPC object."
  (acapella-a2a--with-jsonrpc-url
   profile
   (lambda (url)
     (let* ((headers (acapella-a2a--json-headers profile))
            (payload (acapella-a2a--json
                      (acapella-a2a--rpc-envelope method params))))
       (acapella-transport-http-post
        url headers payload
        (lambda (resp)
          (funcall on-result
                   (acapella-a2a--normalize-jsonrpc
                    (acapella-a2a--http->jsonrpc resp #'identity)))))))))

(defun acapella-a2a--rpc-stream (profile method params on-event on-close)
  "Small orchestrator for SSE: resolve URL, build envelope and open stream.
Parses JSON per SSE data and calls ON-EVENT with decoded object, ignoring corrupt chunks."
  (acapella-a2a--with-jsonrpc-url
   profile
   (lambda (url)
     (let* ((headers (acapella-a2a--sse-headers profile))
            (payload (acapella-a2a--json
                      (acapella-a2a--rpc-envelope method params))))
       (acapella-transport-sse-open
        url headers payload
        (lambda (ev)
          (let ((data (plist-get ev :data)))
            (when (and (stringp data) (not (string-empty-p data)))
              (condition-case _
                  (funcall on-event (acapella-a2a--parse-json data))
                (error
                 (acapella-transport--traffic-log "SSE JSON parse error, raw=%s" data))))))
        on-close)))))

;; Helper: resolve JSON-RPC URL (Agent Card if configured) ---------------------

(defun acapella-a2a--with-jsonrpc-url (profile k)
  "Resolve JSON-RPC URL for PROFILE and call K with URL.
If PROFILE has 'agent-card-url', resolve via Agent Card; otherwise use :url."
  (let ((card-url (cdr (assq 'agent-card-url profile))))
    (if card-url
        (acapella-a2a-resolve-jsonrpc-url profile k)
      (funcall k (acapella--profile-url profile)))))

;; Public API ------------------------------------------------------------------

(defun acapella-a2a-send (profile text on-result)
  "Send TEXT via A2A message/send using PROFILE (alist).
ON-RESULT is called once with decoded JSON object."
  (acapella-a2a--rpc-call
   profile
   "message/send"
   `(("message" . ,(acapella-a2a--text-message text))
     ("metadata" . ,(make-hash-table :test 'equal)))
   on-result))

(defun acapella-a2a-stream (profile text on-event on-close)
  "Stream TEXT via A2A message/stream using PROFILE.
Return SSE handle. ON-EVENT gets decoded JSON object per chunk; ON-CLOSE gets (:exit STRING)."
  (acapella-a2a--rpc-stream
   profile
   "message/stream"
   `(("message" . ,(acapella-a2a--text-message text))
     ("metadata" . ,(make-hash-table :test 'equal)))
   on-event on-close))

;; ---------------------------------------------------------------------------
;; A2A: tasks/get and tasks/cancel (JSON-RPC)

(defun acapella-a2a-get-task (profile task-id on-result)
  "Call A2A JSON-RPC method tasks/get using PROFILE for TASK-ID."
  (unless (and (stringp task-id) (not (string-empty-p task-id)))
    (user-error "[Acapella] Task ID is required"))
  (acapella-a2a--rpc-call
   profile "tasks/get" `(("taskId" . ,task-id)) on-result))

(defun acapella-a2a-cancel (profile task-id on-result)
  "Call A2A JSON-RPC method tasks/cancel using PROFILE for TASK-ID."
  (unless (and (stringp task-id) (not (string-empty-p task-id)))
    (user-error "[Acapella] Task ID is required"))
  (acapella-a2a--rpc-call
   profile "tasks/cancel" `(("taskId" . ,task-id)) on-result))

;; ---------------------------------------------------------------------------
;; A2A: tasks/resubscribe (SSE)

(defun acapella-a2a-resubscribe (profile task-id on-event on-close)
  "Resume SSE stream for TASK-ID via A2A tasks/resubscribe using PROFILE.
Return SSE handle."
  (unless (and (stringp task-id) (not (string-empty-p task-id)))
    (user-error "[Acapella] Task ID is required"))
  (acapella-a2a--rpc-stream
   profile "tasks/resubscribe" `(("taskId" . ,task-id)) on-event on-close))

;; ---------------------------------------------------------------------------
;; Agent Card: fetch + transport selection (JSONRPC)

(defun acapella-a2a--agent-card-url (profile)
  "Return Agent Card URL for PROFILE.
Prefer explicit (agent-card-url . URL); otherwise derive from :url."
  (let* ((explicit (cdr (assq 'agent-card-url profile)))
         (base (cdr (assq 'url profile))))
    (or explicit
        (when (stringp base)
          (concat (string-remove-suffix "/" base) "/.well-known/agent-card.json")))))

;; Simple in-memory cache for Agent Cards (per base URL)
(defvar acapella-a2a--agent-card-cache (make-hash-table :test 'equal)
  "Hash table mapping base URL (string) to a plist:
  (:fetched FLOAT-TIME :card ALIST).")

(defun acapella-a2a--cache-key (profile)
  "Return cache key (string) for PROFILE's main :url."
  (let ((u (cdr (assq 'url profile))))
    (and (stringp u) u)))

(defun acapella-a2a--cache-get-card (profile)
  "Return cached Agent Card plist or nil for PROFILE.
When present, returns plist (:fetched TIME :card ALIST)."
  (let ((key (acapella-a2a--cache-key profile)))
    (and key (gethash key acapella-a2a--agent-card-cache))))

(defun acapella-a2a--cache-put-card (profile card)
  "Store CARD (alist) in cache for PROFILE with current timestamp."
  (let ((key (acapella-a2a--cache-key profile)))
    (when key
      (puthash key (list :fetched (float-time) :card card)
               acapella-a2a--agent-card-cache))))

(defun acapella-a2a--cache-fresh-p (entry)
  "Return non-nil if ENTRY (plist) is fresh under TTL."
  (when entry
    (let* ((fetched (plist-get entry :fetched))
           (ttl (or acapella-agent-card-ttl-seconds 0)))
      (and (numberp fetched)
           (> ttl 0)
           (< (- (float-time) fetched) ttl)))))

(defun acapella-a2a--find-header (headers name)
  "Find header NAME (case-insensitive) in HEADERS alist, return cons or nil."
  (acapella-util-ci-header-find headers name))

;; Small helpers for Agent Card HTTP fetch and handling
(defun acapella-a2a--card-headers (profile)
  "Headers for Agent Card fetch: Accept + auth."
  (append '(("Accept" . "application/json"))
          (acapella--headers-with-auth profile)))

(defun acapella-a2a--handle-card-http-response (profile resp on-result)
  "Parse RESP for Agent Card; on success cache and return card, else fallback to cache or error."
  (let ((status (plist-get resp :status))
        (body   (plist-get resp :body)))
    (if (and status (>= status 200) (< status 300))
        (condition-case err
            (let ((card (acapella-a2a--parse-json body)))
              (acapella-a2a--cache-put-card profile card)
              (funcall on-result card))
          (error (funcall on-result `(("jsonrpc" . "2.0")
                                      ("error" . (("code" . -32700)
                                                  ("message" . ,(error-message-string err))))))))
      (let* ((cached (acapella-a2a--cache-get-card profile))
             (card (plist-get cached :card)))
        (if card
            (funcall on-result card)
          (funcall on-result `(("jsonrpc" . "2.0")
                               ("error" . (("code" . ,(- status))
                                           ("message" . ,(format "HTTP error %s fetching Agent Card" status)))))))))))

(defun acapella-a2a--fetch-card-http (profile url on-result)
  "HTTP GET Agent Card at URL for PROFILE and call ON-RESULT with parsed card or error."
  (acapella-transport-http-get
   url (acapella-a2a--card-headers profile)
   (lambda (resp) (acapella-a2a--handle-card-http-response profile resp on-result))))

(defun acapella-a2a-fetch-agent-card (profile on-result)
  "Fetch Agent Card JSON for PROFILE and call ON-RESULT with parsed alist or error JSON-RPC-like object.
Uses in-memory TTL cache. On HTTP error, returns cached card if available (even if stale)."
  (let* ((card-url (acapella-a2a--agent-card-url profile)))
    (unless card-url
      (user-error "[Acapella] Agent Card URL not configured"))
    (let* ((cached (acapella-a2a--cache-get-card profile)))
      (if (acapella-a2a--cache-fresh-p cached)
          (funcall on-result (plist-get cached :card))
        (acapella-a2a--fetch-card-http profile card-url on-result)))))

(defun acapella-a2a-select-jsonrpc-url (agent-card &optional fallback-url)
  "Given AGENT-CARD (alist), pick JSONRPC URL per A2A §5.6.
Return chosen URL string or FALLBACK-URL."
  (let* ((pref (alist-get "preferredTransport" agent-card nil nil #'string=))
         (main (alist-get "url" agent-card nil nil #'string=))
         (ifaces (alist-get "additionalInterfaces" agent-card nil nil #'string=)))
    (cond
     ((and (stringp pref) (string= pref "JSONRPC") (stringp main)) main)
     ((and (listp ifaces))
      (let ((match (seq-find (lambda (it)
                               (string= (alist-get "transport" it nil nil #'string=) "JSONRPC"))
                             ifaces)))
        (or (alist-get "url" match nil nil #'string=) fallback-url)))
     (t fallback-url))))

(defun acapella-a2a-validate-agent-card (agent-card)
  "Validate minimal requirements of AGENT-CARD per A2A §5.6.
Return nil if valid, or an error-shaped alist ((\"jsonrpc\" . \"2.0\") (\"error\" . ...)) if invalid."
  (let* ((pref (alist-get "preferredTransport" agent-card nil nil #'string=))
         (url  (alist-get "url" agent-card nil nil #'string=)))
    (cond
     ((not (stringp pref))
      '(("jsonrpc" . "2.0")
        ("error" . (("code" . -32006) ("message" . "Agent Card missing preferredTransport")))))
     ((and (string= pref "JSONRPC") (not (stringp url)))
      '(("jsonrpc" . "2.0")
        ("error" . (("code" . -32006) ("message" . "Agent Card missing main url for JSONRPC")))))
     (t nil))))

(defun acapella-a2a-resolve-jsonrpc-url (profile on-result)
  "Fetch Agent Card for PROFILE and call ON-RESULT with chosen JSONRPC URL (string).
Fallback to PROFILE :url on error or invalid Agent Card."
  (let* ((fallback (acapella--profile-url profile)))
    (acapella-a2a-fetch-agent-card
     profile
     (lambda (card-or-error)
       (let ((err (alist-get "error" card-or-error nil nil #'string=)))
         (if err
             (funcall on-result fallback)
           (let ((verr (acapella-a2a-validate-agent-card card-or-error)))
             (if verr
                 (funcall on-result fallback)
               (funcall on-result (acapella-a2a-select-jsonrpc-url card-or-error fallback))))))))))

;; ---------------------------------------------------------------------------
;; Authenticated Extended Agent Card (JSON-RPC)
;;
;; Требует авторизации на уровне HTTP заголовков (см. acapella--headers-with-auth).

(defun acapella-a2a-get-authenticated-card (profile on-result)
  "Fetch authenticated extended Agent Card via JSON-RPC using PROFILE.
Call ON-RESULT with parsed Agent Card alist or error-shaped object (may include WWW-Authenticate in error.data)."
  (acapella-a2a--with-jsonrpc-url
   profile
   (lambda (url)
     (let* ((headers (append '(("Content-Type" . "application/json"))
                             (acapella--headers-with-auth-and-ext profile)))
            (payload (acapella-a2a--json
                      (acapella-a2a--rpc-envelope
                       "agent/getAuthenticatedExtendedCard" nil))))
       (acapella-transport-http-post
        url headers payload
        (lambda (resp)
          (let ((obj (acapella-a2a--normalize-jsonrpc
                      (acapella-a2a--http->jsonrpc resp #'identity))))
            (funcall on-result obj))))))))

;; ---------------------------------------------------------------------------
;; A2A: Push Notification Config (set/get/list/delete)

(defun acapella-a2a-push-set (profile config on-result)
  "Set push notification CONFIG for a task via A2A tasks/pushNotificationConfig/set."
  (let ((task-id (alist-get "taskId" config nil nil #'string=)))
    (unless (and (stringp task-id) (not (string-empty-p task-id)))
      (user-error "[Acapella] config.taskId is required")))
  (acapella-a2a--rpc-call
   profile "tasks/pushNotificationConfig/set" config on-result))

(defun acapella-a2a-push-get (profile task-id config-id on-result)
  "Get push notification config for TASK-ID and CONFIG-ID via A2A tasks/pushNotificationConfig/get."
  (unless (and (stringp task-id) (not (string-empty-p task-id)))
    (user-error "[Acapella] task-id is required"))
  (unless (and (stringp config-id) (not (string-empty-p config-id)))
    (user-error "[Acapella] config-id is required"))
  (acapella-a2a--rpc-call
   profile "tasks/pushNotificationConfig/get"
   `(("taskId" . ,task-id) ("configId" . ,config-id))
   on-result))

(defun acapella-a2a-push-list (profile task-id on-result)
  "List push notification configs for TASK-ID via A2A tasks/pushNotificationConfig/list."
  (unless (and (stringp task-id) (not (string-empty-p task-id)))
    (user-error "[Acapella] task-id is required"))
  (acapella-a2a--rpc-call
   profile "tasks/pushNotificationConfig/list" `(("taskId" . ,task-id)) on-result))

(defun acapella-a2a-push-delete (profile task-id config-id on-result)
  "Delete push notification config CONFIG-ID for TASK-ID via A2A tasks/pushNotificationConfig/delete."
  (unless (and (stringp task-id) (not (string-empty-p task-id)))
    (user-error "[Acapella] task-id is required"))
  (unless (and (stringp config-id) (not (string-empty-p config-id)))
    (user-error "[Acapella] config-id is required"))
  (acapella-a2a--rpc-call
   profile "tasks/pushNotificationConfig/delete"
   `(("taskId" . ,task-id) ("configId" . ,config-id))
   on-result))

(defun acapella-a2a-clear-agent-card-cache (&optional profile)
  "Clear Agent Card in-memory cache. If PROFILE non-nil, clear only its entry."
  (if profile
      (let ((key (acapella-a2a--cache-key profile)))
        (when key (remhash key acapella-a2a--agent-card-cache)))
    (clrhash acapella-a2a--agent-card-cache))
  t)

;; ---------------------------------------------------------------------------
;; A2A: tasks/list (JSON-RPC, optional per spec)

(defun acapella-a2a-list-tasks (profile params on-result)
  "List tasks via A2A JSON-RPC method tasks/list using PROFILE.
PARAMS is an alist (may be nil) passed as JSON-RPC params.
ON-RESULT is called with normalized JSON-RPC object."
  (acapella-a2a--rpc-call
   profile "tasks/list" (or params '()) on-result))

(provide 'acapella-proto-a2a)

;;; acapella-proto-a2a.el ends here
