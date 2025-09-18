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
  "Send TEXT via A2A message/send using PROFILE (alist). Call ON-RESULT with parsed JSON-RPC response alist.
ON-RESULT is called once with the decoded JSON object."
  (acapella-a2a--with-jsonrpc-url
   profile
   (lambda (url)
     (let* ((headers (append '(("Content-Type" . "application/json"))
                             (acapella--headers-with-auth-and-ext profile)))
            (payload (acapella-a2a--json
                      (acapella-a2a--rpc-envelope
                       "message/send"
                       `(("message" . ,(acapella-a2a--text-message text))
                         ("metadata" . ,(make-hash-table :test 'equal)))))))
       (acapella-transport-http-post
        url headers payload
        (lambda (resp)
          (let* ((status (plist-get resp :status))
                 (body (plist-get resp :body)))
            (if (and status (>= status 200) (< status 300))
                (condition-case err
                    (funcall on-result (acapella-a2a--parse-json body))
                  (error (funcall on-result `(("jsonrpc" . "2.0")
                                              ("error" . (("code" . -32700)
                                                          ("message" . ,(error-message-string err))))))))
              (funcall on-result `(("jsonrpc" . "2.0")
                                   ("error" . (("code" . ,(- status))
                                               ("message" . ,(format "HTTP error %s" status))))))))))))))

(defun acapella-a2a-stream (profile text on-event on-close)
  "Stream TEXT via A2A message/stream using PROFILE.
ON-EVENT called with decoded JSON-RPC response alist for every SSE data event.
ON-CLOSE called with plist (:exit STRING).
Return an SSE handle."
  (acapella-a2a--with-jsonrpc-url
   profile
   (lambda (url)
     (let* ((headers (append '(("Accept" . "text/event-stream")
                               ("Content-Type" . "application/json"))
                             (acapella--headers-with-auth-and-ext profile)))
            (payload (acapella-a2a--json
                      (acapella-a2a--rpc-envelope
                       "message/stream"
                       `(("message" . ,(acapella-a2a--text-message text))
                         ("metadata" . ,(make-hash-table :test 'equal)))))))
       (acapella-transport-sse-open
        url headers payload
        (lambda (ev)
          (let ((data (plist-get ev :data)))
            (when (and (stringp data) (not (string-empty-p data)))
              (condition-case _
                  (funcall on-event (acapella-a2a--parse-json data))
                (error
                 ;; ignore individual corrupt events, but still show raw traffic
                 (acapella-transport--traffic-log "SSE JSON parse error, raw=%s" data))))))
        on-close)))))

;; ---------------------------------------------------------------------------
;; A2A: tasks/get and tasks/cancel (JSON-RPC)

(defun acapella-a2a-get-task (profile task-id on-result)
  "Call A2A JSON-RPC method tasks/get using PROFILE for TASK-ID.
ON-RESULT is called once with parsed JSON alist (or error object)."
  (unless (and (stringp task-id) (not (string-empty-p task-id)))
    (user-error "[Acapella] Task ID is required"))
  (acapella-a2a--with-jsonrpc-url
   profile
   (lambda (url)
     (let* ((headers (append '(("Content-Type" . "application/json"))
                             (acapella--headers-with-auth-and-ext profile)))
            (payload (acapella-a2a--json
                      (acapella-a2a--rpc-envelope
                       "tasks/get"
                       `(("taskId" . ,task-id))))))
       (acapella-transport-http-post
        url headers payload
        (lambda (resp)
          (let ((status (plist-get resp :status))
                (body   (plist-get resp :body)))
            (if (and status (>= status 200) (< status 300))
                (condition-case err
                    (funcall on-result (acapella-a2a--parse-json body))
                  (error (funcall on-result `(("jsonrpc" . "2.0")
                                              ("error" . (("code" . -32700)
                                                          ("message" . ,(error-message-string err))))))))
              (funcall on-result `(("jsonrpc" . "2.0")
                                   ("error" . (("code" . ,(- status))
                                               ("message" . ,(format "HTTP error %s" status))))))))))))))

(defun acapella-a2a-cancel (profile task-id on-result)
  "Call A2A JSON-RPC method tasks/cancel using PROFILE for TASK-ID.
ON-RESULT is called once with parsed JSON alist (or error object)."
  (unless (and (stringp task-id) (not (string-empty-p task-id)))
    (user-error "[Acapella] Task ID is required"))
  (acapella-a2a--with-jsonrpc-url
   profile
   (lambda (url)
     (let* ((headers (append '(("Content-Type" . "application/json"))
                             (acapella--headers-with-auth-and-ext profile)))
            (payload (acapella-a2a--json
                      (acapella-a2a--rpc-envelope
                       "tasks/cancel"
                       `(("taskId" . ,task-id))))))
       (acapella-transport-http-post
        url headers payload
        (lambda (resp)
          (let ((status (plist-get resp :status))
                (body   (plist-get resp :body)))
            (if (and status (>= status 200) (< status 300))
                (condition-case err
                    (funcall on-result (acapella-a2a--parse-json body))
                  (error (funcall on-result `(("jsonrpc" . "2.0")
                                              ("error" . (("code" . -32700)
                                                          ("message" . ,(error-message-string err))))))))
              (funcall on-result `(("jsonrpc" . "2.0")
                                   ("error" . (("code" . ,(- status))
                                               ("message" . ,(format "HTTP error %s" status))))))))))))))

;; ---------------------------------------------------------------------------
;; A2A: tasks/resubscribe (SSE)

(defun acapella-a2a-resubscribe (profile task-id on-event on-close)
  "Resume SSE stream for TASK-ID via A2A tasks/resubscribe using PROFILE.
ON-EVENT called with decoded JSON-RPC response alist per SSE data.
ON-CLOSE called with plist (:exit STRING). Return SSE handle."
  (unless (and (stringp task-id) (not (string-empty-p task-id)))
    (user-error "[Acapella] Task ID is required"))
  (acapella-a2a--with-jsonrpc-url
   profile
   (lambda (url)
     (let* ((headers (append '(("Accept" . "text/event-stream")
                               ("Content-Type" . "application/json"))
                             (acapella--headers-with-auth-and-ext profile)))
            (payload (acapella-a2a--json
                      (acapella-a2a--rpc-envelope
                       "tasks/resubscribe"
                       `(("taskId" . ,task-id))))))
       (acapella-transport-sse-open
        url headers payload
        (lambda (ev)
          (let ((data (plist-get ev :data)))
            (when (and (stringp data) (not (string-empty-p data)))
              (condition-case _
                  (funcall on-event (acapella-a2a--parse-json data))
                (error
                 (acapella-transport--traffic-log "SSE JSON parse error (resubscribe), raw=%s" data))))))
        on-close)))))

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

(defun acapella-a2a-fetch-agent-card (profile on-result)
  "Fetch Agent Card JSON for PROFILE and call ON-RESULT with parsed alist or error JSON-RPC-like object."
  (let* ((card-url (acapella-a2a--agent-card-url profile)))
    (unless card-url
      (user-error "[Acapella] Agent Card URL not configured"))
    (let ((headers (append '(("Accept" . "application/json"))
                           (acapella--headers-with-auth profile))))
      (acapella-transport-http-get
       card-url headers
       (lambda (resp)
         (let ((status (plist-get resp :status))
               (body   (plist-get resp :body)))
           (if (and status (>= status 200) (< status 300))
               (condition-case err
                   (funcall on-result (acapella-a2a--parse-json body))
                 (error (funcall on-result `(("jsonrpc" . "2.0")
                                             ("error" . (("code" . -32700)
                                                         ("message" . ,(error-message-string err))))))))
             (funcall on-result `(("jsonrpc" . "2.0")
                                  ("error" . (("code" . ,(- status))
                                              ("message" . ,(format "HTTP error %s fetching Agent Card" status)))))))))))))

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

(defun acapella-a2a-resolve-jsonrpc-url (profile on-result)
  "Fetch Agent Card for PROFILE and call ON-RESULT with chosen JSONRPC URL (string).
Fallback to PROFILE :url on error."
  (let* ((fallback (acapella--profile-url profile)))
    (acapella-a2a-fetch-agent-card
     profile
     (lambda (card-or-error)
       (let ((err (alist-get "error" card-or-error nil nil #'string=)))
         (if err
             (funcall on-result fallback)
           (funcall on-result (acapella-a2a-select-jsonrpc-url card-or-error fallback))))))))

;; ---------------------------------------------------------------------------
;; Authenticated Extended Agent Card (JSON-RPC)
;;
;; Требует авторизации на уровне HTTP заголовков (см. acapella--headers-with-auth).

(defun acapella-a2a-get-authenticated-card (profile on-result)
  "Fetch authenticated extended Agent Card via JSON-RPC using PROFILE.
Call ON-RESULT with parsed Agent Card alist or error-shaped object."
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
          (let ((status (plist-get resp :status))
                (body   (plist-get resp :body)))
            (if (and status (>= status 200) (< status 300))
                (condition-case err
                    (funcall on-result (acapella-a2a--parse-json body))
                  (error (funcall on-result `(("jsonrpc" . "2.0")
                                              ("error" . (("code" . -32700)
                                                          ("message" . ,(error-message-string err))))))))
              (funcall on-result `(("jsonrpc" . "2.0")
                                   ("error" . (("code" . ,(- status))
                                               ("message" . ,(format "HTTP error %s fetching Authenticated Card" status))))))))))))))

;; ---------------------------------------------------------------------------
;; A2A: Push Notification Config (set/get/list/delete)

(defun acapella-a2a-push-set (profile config on-result)
  "Set push notification CONFIG for a task via A2A tasks/pushNotificationConfig/set.
CONFIG is an alist representing TaskPushNotificationConfig (must include \"taskId\").
Call ON-RESULT with parsed JSON-RPC response."
  (let ((task-id (alist-get "taskId" config nil nil #'string=)))
    (unless (and (stringp task-id) (not (string-empty-p task-id)))
      (user-error "[Acapella] config.taskId is required")))
  (acapella-a2a--with-jsonrpc-url
   profile
   (lambda (url)
     (let* ((headers (append '(("Content-Type" . "application/json"))
                             (acapella--headers-with-auth-and-ext profile)))
            (payload (acapella-a2a--json
                      (acapella-a2a--rpc-envelope
                       "tasks/pushNotificationConfig/set" config))))
       (acapella-transport-http-post
        url headers payload
        (lambda (resp)
          (let ((status (plist-get resp :status))
                (body   (plist-get resp :body)))
            (if (and status (>= status 200) (< status 300))
                (condition-case err
                    (funcall on-result (acapella-a2a--parse-json body))
                  (error (funcall on-result `(("jsonrpc" . "2.0")
                                              ("error" . (("code" . -32700)
                                                          ("message" . ,(error-message-string err))))))))
              (funcall on-result `(("jsonrpc" . "2.0")
                                   ("error" . (("code" . ,(- status))
                                               ("message" . ,(format "HTTP error %s" status))))))))))))))

(defun acapella-a2a-push-get (profile task-id config-id on-result)
  "Get push notification config for TASK-ID and CONFIG-ID via A2A tasks/pushNotificationConfig/get."
  (unless (and (stringp task-id) (not (string-empty-p task-id)))
    (user-error "[Acapella] task-id is required"))
  (unless (and (stringp config-id) (not (string-empty-p config-id)))
    (user-error "[Acapella] config-id is required"))
  (acapella-a2a--with-jsonrpc-url
   profile
   (lambda (url)
     (let* ((headers (append '(("Content-Type" . "application/json"))
                             (acapella--headers-with-auth-and-ext profile)))
            (params `(("taskId" . ,task-id) ("configId" . ,config-id)))
            (payload (acapella-a2a--json
                      (acapella-a2a--rpc-envelope
                       "tasks/pushNotificationConfig/get" params))))
       (acapella-transport-http-post
        url headers payload
        (lambda (resp)
          (let ((status (plist-get resp :status))
                (body   (plist-get resp :body)))
            (if (and status (>= status 200) (< status 300))
                (condition-case err
                    (funcall on-result (acapella-a2a--parse-json body))
                  (error (funcall on-result `(("jsonrpc" . "2.0")
                                              ("error" . (("code" . -32700)
                                                          ("message" . ,(error-message-string err))))))))
              (funcall on-result `(("jsonrpc" . "2.0")
                                   ("error" . (("code" . ,(- status))
                                               ("message" . ,(format "HTTP error %s" status))))))))))))))

(defun acapella-a2a-push-list (profile task-id on-result)
  "List push notification configs for TASK-ID via A2A tasks/pushNotificationConfig/list."
  (unless (and (stringp task-id) (not (string-empty-p task-id)))
    (user-error "[Acapella] task-id is required"))
  (acapella-a2a--with-jsonrpc-url
   profile
   (lambda (url)
     (let* ((headers (append '(("Content-Type" . "application/json"))
                             (acapella--headers-with-auth-and-ext profile)))
            (params `(("taskId" . ,task-id)))
            (payload (acapella-a2a--json
                      (acapella-a2a--rpc-envelope
                       "tasks/pushNotificationConfig/list" params))))
       (acapella-transport-http-post
        url headers payload
        (lambda (resp)
          (let ((status (plist-get resp :status))
                (body   (plist-get resp :body)))
            (if (and status (>= status 200) (< status 300))
                (condition-case err
                    (funcall on-result (acapella-a2a--parse-json body))
                  (error (funcall on-result `(("jsonrpc" . "2.0")
                                              ("error" . (("code" . -32700)
                                                          ("message" . ,(error-message-string err))))))))
              (funcall on-result `(("jsonrpc" . "2.0")
                                   ("error" . (("code" . ,(- status))
                                               ("message" . ,(format "HTTP error %s" status))))))))))))))

(defun acapella-a2a-push-delete (profile task-id config-id on-result)
  "Delete push notification config CONFIG-ID for TASK-ID via A2A tasks/pushNotificationConfig/delete."
  (unless (and (stringp task-id) (not (string-empty-p task-id)))
    (user-error "[Acapella] task-id is required"))
  (unless (and (stringp config-id) (not (string-empty-p config-id)))
    (user-error "[Acapella] config-id is required"))
  (acapella-a2a--with-jsonrpc-url
   profile
   (lambda (url)
     (let* ((headers (append '(("Content-Type" . "application/json"))
                             (acapella--headers-with-auth-and-ext profile)))
            (params `(("taskId" . ,task-id) ("configId" . ,config-id)))
            (payload (acapella-a2a--json
                      (acapella-a2a--rpc-envelope
                       "tasks/pushNotificationConfig/delete" params))))
       (acapella-transport-http-post
        url headers payload
        (lambda (resp)
          (let ((status (plist-get resp :status))
                (body   (plist-get resp :body)))
            (if (and status (>= status 200) (< status 300))
                (condition-case err
                    (funcall on-result (acapella-a2a--parse-json body))
                  (error (funcall on-result `(("jsonrpc" . "2.0")
                                              ("error" . (("code" . -32700)
                                                          ("message" . ,(error-message-string err))))))))
              (funcall on-result `(("jsonrpc" . "2.0")
                                   ("error" . (("code" . ,(- status))
                                               ("message" . ,(format "HTTP error %s" status))))))))))))))

(provide 'acapella-proto-a2a)

;;; acapella-proto-a2a.el ends here
