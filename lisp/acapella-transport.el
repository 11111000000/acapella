;;; acapella-transport.el --- Transport (HTTP + SSE) for Acapella -*- lexical-binding: t; -*-

;; Author: Acapella
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.2"))
;; Keywords: http, sse, json
;; URL: https://example.com/acapella

;;; Commentary:
;; Minimal, effect-like transport:
;; - HTTP POST (url.el) for JSON-RPC
;; - SSE via curl process (text/event-stream), with simple parser
;; Functions are small and focused; logging is optional and light.

;;; Code:

(require 'url)
(require 'json)
(require 'subr-x)
(require 'acapella-util)

(defgroup acapella-transport nil
  "Transport layer for Acapella."
  :group 'acapella)

(defcustom acapella-sse-auto-reconnect t
  "When non-nil, automatically attempt to reconnect SSE streams on unexpected close."
  :type 'boolean
  :group 'acapella-transport)

(defcustom acapella-sse-reconnect-delay-seconds 2
  "Delay in seconds before attempting SSE reconnect."
  :type 'integer
  :group 'acapella-transport)

(defcustom acapella-sse-reconnect-max 5
  "Maximum number of SSE auto-reconnect attempts."
  :type 'integer
  :group 'acapella-transport)

(defcustom acapella-sse-reconnect-backoff 'exponential
  "Backoff strategy for SSE auto-reconnect: 'linear or 'exponential."
  :type '(choice (const :tag "Linear" linear)
                 (const :tag "Exponential" exponential))
  :group 'acapella-transport)

(defcustom acapella-sse-reconnect-backoff-factor 2.0
  "Backoff factor for SSE auto-reconnect when `acapella-sse-reconnect-backoff' is 'exponential."
  :type 'number
  :group 'acapella-transport)

(defcustom acapella-traffic-buffer "*Acapella Traffic*"
  "Name of buffer to log transport events."
  :type 'string
  :group 'acapella-transport)

(defcustom acapella-traffic-log-level 'info
  "Traffic log level: nil (off), 'info, or 'debug."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "Info" info)
                 (const :tag "Debug" debug))
  :group 'acapella-transport)

(defun acapella-transport--traffic-log (fmt &rest args)
  "Append a formatted message to traffic buffer."
  (when (and acapella-traffic-buffer acapella-traffic-log-level)
    (with-current-buffer (get-buffer-create acapella-traffic-buffer)
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
        (insert (apply #'format fmt args))
        (insert "\n")
        (acapella-transport--traffic-trim!)))))

(defun acapella-transport--log-info (fmt &rest args)
  "Log INFO-level traffic (shown when level is info or debug)."
  (when (memq acapella-traffic-log-level '(info debug))
    (apply #'acapella-transport--traffic-log fmt args)))

(defun acapella-transport--log-debug (fmt &rest args)
  "Log DEBUG-level traffic (shown only when level is debug)."
  (when (eq acapella-traffic-log-level 'debug)
    (apply #'acapella-transport--traffic-log fmt args)))

(defcustom acapella-traffic-max-bytes 262144
  "Maximum size (in bytes) of the traffic buffer.
When the buffer exceeds this size, older log lines are trimmed."
  :type 'integer
  :group 'acapella-transport)

(defun acapella-transport--traffic-trim! ()
  "Trim the traffic buffer to `acapella-traffic-max-bytes' if it grows larger."
  (when (and acapella-traffic-buffer acapella-traffic-max-bytes)
    (with-current-buffer (get-buffer-create acapella-traffic-buffer)
      (when (> (buffer-size) acapella-traffic-max-bytes)
        (let ((inhibit-read-only t)
              (excess (- (buffer-size) acapella-traffic-max-bytes)))
          (goto-char (point-min))
          (delete-region (point-min) (min (point-max) (+ excess 1)))
          (goto-char (point-min))
          (insert "[Acapella] ... trimmed older logs ...\n"))))))

(defun acapella-transport-open-traffic ()
  "Open the transport traffic buffer."
  (interactive)
  (pop-to-buffer (get-buffer-create acapella-traffic-buffer)))

(defun acapella-transport--alist->headers (headers)
  "Convert HEADERS alist ((\"Name\" . \"Value\") ...) to list of strings \"Name: Value\"."
  (mapcar (lambda (kv) (format "%s: %s" (car kv) (cdr kv))) headers))

(defun acapella-transport--parse-headers-from-string (header-region)
  "Parse HEADER-REGION (string) into an alist of headers (NAME . VALUE)."
  (let ((hdrs '()))
    (dolist (l (split-string header-region "\r?\n" t))
      (when (string-match "\\`\\([^:]+\\):[ \t]*\\(.*\\)\\'" l)
        (push (cons (match-string 1 l) (match-string 2 l)) hdrs)))
    (nreverse hdrs)))

(defun acapella-transport--extract-content-type (headers)
  "Return Content-Type value (string) from HEADERS alist, or nil."
  (cdr (acapella-util-ci-header-find headers "Content-Type")))

(defun acapella-transport--fmt-curl-headers (headers)
  "Return flat list of \"-H\" and \"Name: Value\" pairs for curl from HEADERS alist."
  (apply #'append
         (mapcar (lambda (kv)
                   (list "-H" (format "%s: %s" (car kv) (cdr kv))))
                 headers)))

(defun acapella-transport--reconnect-delay (attempt)
  "Compute reconnect delay (seconds) for SSE based on ATTEMPT (1-based).
Uses `acapella-sse-reconnect-backoff' and `acapella-sse-reconnect-delay-seconds'."
  (max 0
       (cond
        ((eq acapella-sse-reconnect-backoff 'linear)
         (* acapella-sse-reconnect-delay-seconds (max 1 attempt)))
        (t ;; exponential by default
         (truncate (* acapella-sse-reconnect-delay-seconds
                      (expt (or acapella-sse-reconnect-backoff-factor 2.0)
                            (max 0 (1- attempt)))))))))

(defun acapella-transport--curl-args (url headers data)
  "Build curl ARGS for SSE POST to URL with HEADERS and DATA."
  (append
   (list "-sS" "-N" "--no-buffer")
   ;; Always include SSE defaults
   (acapella-transport--fmt-curl-headers
    (append '(("Accept" . "text/event-stream")
              ("Content-Type" . "application/json"))
            headers))
   (list "-X" "POST" "-d" data url)))

(defun acapella-transport-http-post (url headers body on-done)
  "HTTP POST to URL with HEADERS alist and BODY string. ON-DONE gets (plist :status CODE :headers H :body S).
Asynchronous; ON-DONE called in a temporary buffer context."
  (let* ((url-request-method "POST")
         (url-request-extra-headers headers)
         (url-request-data (encode-coding-string body 'utf-8)))
    (acapella-transport--traffic-log "HTTP POST %s headers=%s body-len=%d"
                                     url
                                     (mapconcat (lambda (kv)
                                                  (format "%s: %s"
                                                          (car kv)
                                                          (acapella-util-mask-header (car kv) (cdr kv))))
                                                headers ", ")
                                     (length body))
    (url-retrieve
     url
     (lambda (status)
       (let ((err (plist-get status :error))
             (code (or (bound-and-true-p url-http-response-status) 0))
             (hdrs '()))
         (goto-char (point-min))
         ;; Collect raw headers
         (when (re-search-forward "\r?\n\r?\n" nil t)
           (let ((header-region (buffer-substring (point-min) (match-beginning 0))))
             (setq hdrs (acapella-transport--parse-headers-from-string header-region))))
         (let* ((body (buffer-substring-no-properties (point) (point-max)))
                (resp (list :status code
                            :headers hdrs
                            :content-type (acapella-transport--extract-content-type hdrs)
                            :body body :error err)))
           (acapella-transport--traffic-log "HTTP RESP %s code=%s len=%d err=%S"
                                            url code (length body) err)
           (funcall on-done resp))
         (kill-buffer (current-buffer))))
     nil t t)))

;; SSE via curl subprocess -----------------------------------------------------

(cl-defstruct (acapella-transport-sse
               (:constructor acapella-transport--make-sse))
  process
  buffer
  acc
  on-event
  on-close
  url
  headers
  payload
  last-id
  retries
  auto-reconnect
  server-retry-ms)

(defun acapella-transport--parse-sse-events (acc chunk)
  "Accumulate CHUNK into ACC string and return (cons NEW-ACC events-list).
Each event is an alist: ((id . STRING|nil) (data . STRING|nil))."
  (let ((new (concat (or acc "") chunk))
        (events '()))
    (while (string-match "\\(\r?\n\\)\\1" new)
      (let* ((split-idx (match-end 0))
             (raw (substring new 0 (- split-idx (length (match-string 0 new)))))
             (rest (substring new split-idx))
             (id nil)
             (data-lines '()))
        ;; parse raw block lines
        (dolist (line (split-string raw "\r?\n"))
          (cond
           ((string-prefix-p "data:" line)
            (push (string-trim (substring line 5)) data-lines))
           ((string-prefix-p "id:" line)
            (setq id (string-trim (substring line 3))))
           (t ;; ignore others: event:, retry:, comments
            )))
        (push (list (cons 'id id)
                    (cons 'data (when data-lines
                                  (mapconcat #'identity (nreverse data-lines) "\n"))))
              events)
        (setq new rest)))
    (cons new (nreverse events))))

(defun acapella-transport-sse-open (url headers data on-event on-close)
  "Open SSE stream by POSTing DATA JSON to URL with HEADERS (alist).
ON-EVENT is called with (plist :id ID :data STR), ON-CLOSE with (plist :exit STATUS).
Return an SSE handle object."
  (let* ((curl (executable-find "curl")))
    (unless curl
      (user-error "[Acapella] curl not found. Please install curl for SSE"))
    (let* ((buf (generate-new-buffer " *acapella-sse*"))
           (args (acapella-transport--curl-args url headers data))
           (proc (make-process
                  :name "acapella-sse"
                  :buffer buf
                  :command (cons curl args)
                  :connection-type 'pipe
                  :noquery t)))
      (acapella-transport--traffic-log "SSE OPEN %s headers=%s body-len=%d"
                                       url
                                       (mapconcat (lambda (kv)
                                                    (format "%s: %s"
                                                            (car kv)
                                                            (acapella-util-mask-header (car kv) (cdr kv))))
                                                  headers ", ")
                                       (length data))
      (let ((handle (acapella-transport--make-sse
                     :process proc :buffer buf :acc ""
                     :on-event on-event :on-close on-close
                     :url url :headers headers
                     :payload data :last-id nil :retries 0
                     :auto-reconnect acapella-sse-auto-reconnect)))
        (set-process-filter
         proc
         (lambda (_proc chunk)
           ;; Capture server-provided retry: N (ms) hints if present
           (let ((start 0))
             (while (string-match "^[ \t]*retry:[ \t]*\\([0-9]+\\)" chunk start)
               (let ((ms (string-to-number (match-string 1 chunk))))
                 (when (> ms 0)
                   (setf (acapella-transport-sse-server-retry-ms handle) ms)))
               (setq start (match-end 0))))
           (pcase-let* ((`(,acc2 . ,events) (acapella-transport--parse-sse-events (acapella-transport-sse-acc handle) chunk)))
             (setf (acapella-transport-sse-acc handle) acc2)
             (dolist (ev events)
               (let* ((id (cdr (assq 'id ev)))
                      (data (cdr (assq 'data ev))))
                 (when id (setf (acapella-transport-sse-last-id handle) id))
                 (acapella-transport--log-debug "SSE EVENT %s id=%s len=%s"
                                                url (or id "nil") (if data (length data) 0))
                 (when data
                   (funcall on-event (list :id id :data data))))))))
        (set-process-sentinel
         proc
         (lambda (_proc event)
           (acapella-transport--traffic-log "SSE CLOSE %s %s" url (string-trim event))
           (let* ((finished (string-match-p (rx (or "finished" "done")) event))
                  (can-reconnect (and (not finished)
                                      (acapella-transport-sse-auto-reconnect handle)
                                      (< (or (acapella-transport-sse-retries handle) 0)
                                         acapella-sse-reconnect-max)))
                  (next-attempt (and can-reconnect
                                     (1+ (or (acapella-transport-sse-retries handle) 0))))
                  (server-ms (acapella-transport-sse-server-retry-ms handle))
                  (delay (and next-attempt
                              (if (and server-ms (> server-ms 0))
                                  (/ server-ms 1000.0)
                                (acapella-transport--reconnect-delay next-attempt)))))
             (when can-reconnect
               (acapella-transport--traffic-log "SSE RECONNECT scheduled after %ss (attempt %s/%s)"
                                                delay next-attempt acapella-sse-reconnect-max)
               (run-at-time delay nil #'acapella-transport--sse-reopen handle))
             (when on-close
               (funcall on-close (append (list :exit event)
                                         (when can-reconnect
                                           (list :reconnect t :attempt next-attempt
                                                 :max acapella-sse-reconnect-max :delay delay))))))
           (when (buffer-live-p buf) (kill-buffer buf))))
        handle))))

(defun acapella-transport--sse-reopen (handle)
  "Re-open SSE for HANDLE using Last-Event-ID if available.
Increments retry counter and resets process/buffer/acc."
  (let* ((curl (executable-find "curl"))
         (url  (acapella-transport-sse-url handle))
         (base-h (acapella-transport-sse-headers handle))
         (last (acapella-transport-sse-last-id handle))
         (payload (acapella-transport-sse-payload handle))
         (headers (if last (cons (cons "Last-Event-ID" last) base-h) base-h))
         (buf (generate-new-buffer " *acapella-sse*"))
         (args (acapella-transport--curl-args url headers payload))
         (proc (make-process
                :name "acapella-sse"
                :buffer buf
                :command (cons curl args)
                :connection-type 'pipe
                :noquery t)))
    (setf (acapella-transport-sse-retries handle)
          (1+ (or (acapella-transport-sse-retries handle) 0)))
    (setf (acapella-transport-sse-process handle) proc)
    (setf (acapella-transport-sse-buffer handle) buf)
    (setf (acapella-transport-sse-acc handle) "")
    (set-process-filter
     proc
     (lambda (_proc chunk)
       ;; Capture server-provided retry: N (ms) hints if present
       (let ((start 0))
         (while (string-match "^[ \t]*retry:[ \t]*\\([0-9]+\\)" chunk start)
           (let ((ms (string-to-number (match-string 1 chunk))))
             (when (> ms 0)
               (setf (acapella-transport-sse-server-retry-ms handle) ms)))
           (setq start (match-end 0))))
       (pcase-let* ((`(,acc2 . ,events) (acapella-transport--parse-sse-events (acapella-transport-sse-acc handle) chunk)))
         (setf (acapella-transport-sse-acc handle) acc2)
         (dolist (ev events)
           (let* ((id (cdr (assq 'id ev)))
                  (data (cdr (assq 'data ev))))
             (when id (setf (acapella-transport-sse-last-id handle) id))
             (acapella-transport--log-debug "SSE EVENT %s id=%s len=%s (reopen)"
                                            url (or id "nil") (if data (length data) 0))
             (when data
               (funcall (acapella-transport-sse-on-event handle) (list :id id :data data))))))))
    (set-process-sentinel
     proc
     (lambda (_proc event)
       (acapella-transport--traffic-log "SSE CLOSE %s %s (reopen)" url (string-trim event))
       (let* ((finished (string-match-p (rx (or "finished" "done")) event))
              (can-reconnect (and (not finished)
                                  (acapella-transport-sse-auto-reconnect handle)
                                  (< (or (acapella-transport-sse-retries handle) 0)
                                     acapella-sse-reconnect-max)))
              (next-attempt (and can-reconnect
                                 (1+ (or (acapella-transport-sse-retries handle) 0))))
              (server-ms (acapella-transport-sse-server-retry-ms handle))
              (delay (and next-attempt
                          (if (and server-ms (> server-ms 0))
                              (/ server-ms 1000.0)
                            (acapella-transport--reconnect-delay next-attempt)))))
         (when can-reconnect
           (run-at-time delay nil #'acapella-transport--sse-reopen handle))
         (when (acapella-transport-sse-on-close handle)
           (funcall (acapella-transport-sse-on-close handle)
                    (append (list :exit event)
                            (when can-reconnect
                              (list :reconnect t :attempt next-attempt
                                    :max acapella-sse-reconnect-max :delay delay))))))
       (when (buffer-live-p buf) (kill-buffer buf))))))

(defun acapella-transport-sse-close (handle)
  "Close SSE HANDLE."
  (when (and handle (process-live-p (acapella-transport-sse-process handle)))
    (delete-process (acapella-transport-sse-process handle)))
  (when (buffer-live-p (acapella-transport-sse-buffer handle))
    (kill-buffer (acapella-transport-sse-buffer handle))))

;; Simple HTTP GET -------------------------------------------------------------

(defun acapella-transport-http-get (url headers on-done)
  "HTTP GET to URL with HEADERS alist. ON-DONE gets (plist :status CODE :headers H :body S).
Asynchronous; ON-DONE called in a temporary buffer context."
  (let* ((url-request-method "GET")
         (url-request-extra-headers headers)
         (url-request-data nil))
    (acapella-transport--traffic-log "HTTP GET %s headers=%s"
                                     url
                                     (mapconcat (lambda (kv)
                                                  (format "%s: %s"
                                                          (car kv)
                                                          (acapella-util-mask-header (car kv) (cdr kv))))
                                                headers ", "))
    (url-retrieve
     url
     (lambda (status)
       (let ((err (plist-get status :error))
             (code (or (bound-and-true-p url-http-response-status) 0))
             (hdrs '()))
         (goto-char (point-min))
         (when (re-search-forward "\r?\n\r?\n" nil t)
           (let ((header-region (buffer-substring (point-min) (match-beginning 0))))
             (setq hdrs (acapella-transport--parse-headers-from-string header-region))))
         (let* ((body (buffer-substring-no-properties (point) (point-max)))
                (resp (list :status code
                            :headers hdrs
                            :content-type (acapella-transport--extract-content-type hdrs)
                            :body body :error err)))
           (acapella-transport--traffic-log "HTTP RESP %s code=%s len=%d err=%S"
                                            url code (length body) err)
           (funcall on-done resp))
         (kill-buffer (current-buffer))))
     nil t t)))

(defun acapella-transport--extract-content-length (headers)
  "Return Content-Length value from HEADERS alist as integer, or nil."
  (when headers
    (let ((val (cdr (acapella-util-ci-header-find headers "Content-Length"))))
      (when (and val (stringp val))
        (condition-case nil
            (string-to-number val)
          (error nil))))))

(defun acapella-transport--http-head (url headers on-done)
  "Perform HTTP HEAD to URL with HEADERS.
ON-DONE is called with a plist similar to http-get: (:status CODE :headers H :content-type CT)."
  (let* ((url-request-method "HEAD")
         (url-request-extra-headers headers)
         (url-request-data nil))
    (acapella-transport--traffic-log "HTTP HEAD %s" url)
    (url-retrieve
     url
     (lambda (status)
       (let ((code (or (bound-and-true-p url-http-response-status) 0))
             (hdrs '()))
         (goto-char (point-min))
         (when (re-search-forward "\r?\n\r?\n" nil t)
           (let ((header-region (buffer-substring (point-min) (match-beginning 0))))
             (setq hdrs (acapella-transport--parse-headers-from-string header-region))))
         (let ((resp (list :status code
                           :headers hdrs
                           :content-type (acapella-transport--extract-content-type hdrs))))
           (acapella-transport--traffic-log "HTTP HEAD RESP %s code=%s" url code)
           (funcall on-done resp))
         (kill-buffer (current-buffer))))
     nil t t)))

(defun acapella-transport-http-download (url headers max-bytes on-done)
  "Download content from URL with HEADERS.
If MAX-BYTES is non-nil and integer, attempt a HEAD first to check Content-Length:
- if Content-Length > MAX-BYTES, call ON-DONE with (:too-large t ...) and avoid downloading;
- otherwise perform GET and return full response (or :too-large if actual body exceeds MAX-BYTES).

ON-DONE is called with plist:
  (:status CODE :headers H :content-type CT :body BYTES :too-large T?)

This keeps behavior compatible with previous implementation when MAX-BYTES is nil."
  (if (and (integerp max-bytes) (> max-bytes 0))
      ;; Try HEAD first to avoid downloading large bodies when possible
      (acapella-transport--http-head
       url headers
       (lambda (head-resp)
         (let ((len (acapella-transport--extract-content-length (plist-get head-resp :headers))))
           (cond
            ((and (integerp len) (> len max-bytes))
             (funcall on-done (append (list :too-large t) head-resp)))
            (t
             ;; Proceed with GET; on completion, still guard against actual body size
             (acapella-transport-http-get
              url headers
              (lambda (resp)
                (let* ((body (plist-get resp :body))
                       (len2 (and (stringp body) (length body))))
                  (if (and (integerp max-bytes) len2 (> len2 max-bytes))
                      (funcall on-done (append (list :too-large t) resp))
                    (funcall on-done resp))))))))))

    ;; No max specified — behave as before
    (acapella-transport-http-get
     url headers
     (lambda (resp)
       (let* ((body (plist-get resp :body))
              (len (and (stringp body) (length body))))
         (if (and (integerp max-bytes) len (> len max-bytes))
             (funcall on-done (append (list :too-large t) resp))
           (funcall on-done resp)))))))

(provide 'acapella-transport)

;;; acapella-transport.el ends here
