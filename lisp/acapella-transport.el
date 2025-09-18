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

(defcustom acapella-traffic-buffer "*Acapella Traffic*"
  "Name of buffer to log transport events."
  :type 'string
  :group 'acapella-transport)

(defun acapella-transport--traffic-log (fmt &rest args)
  "Append a formatted message to traffic buffer."
  (when acapella-traffic-buffer
    (with-current-buffer (get-buffer-create acapella-traffic-buffer)
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
        (insert (apply #'format fmt args))
        (insert "\n")
        (acapella-transport--traffic-trim!)))))

(defvar acapella-traffic-max-bytes 262144
  "Maximum size (bytes) of the traffic buffer. Older logs are trimmed when exceeded.")

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
  auto-reconnect)

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
           (pcase-let* ((`(,acc2 . ,events) (acapella-transport--parse-sse-events (acapella-transport-sse-acc handle) chunk)))
             (setf (acapella-transport-sse-acc handle) acc2)
             (dolist (ev events)
               (let* ((id (cdr (assq 'id ev)))
                      (data (cdr (assq 'data ev))))
                 (when id (setf (acapella-transport-sse-last-id handle) id))
                 (acapella-transport--traffic-log "SSE EVENT %s id=%s len=%s"
                                                  url (or id "nil") (if data (length data) 0))
                 (when data
                   (funcall on-event (list :id id :data data))))))))
        (set-process-sentinel
         proc
         (lambda (_proc event)
           (acapella-transport--traffic-log "SSE CLOSE %s %s" url (string-trim event))
           (let ((finished (string-match-p (rx (or "finished" "done")) event)))
             (when (and (not finished)
                        (acapella-transport-sse-auto-reconnect handle)
                        (< (or (acapella-transport-sse-retries handle) 0) acapella-sse-reconnect-max))
               (acapella-transport--traffic-log "SSE RECONNECT scheduled after %ss (attempt %s/%s)"
                                                acapella-sse-reconnect-delay-seconds
                                                (1+ (or (acapella-transport-sse-retries handle) 0))
                                                acapella-sse-reconnect-max)
               (run-at-time
                acapella-sse-reconnect-delay-seconds nil
                #'acapella-transport--sse-reopen handle)))
           (when on-close
             (funcall on-close (list :exit event)))
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
       (pcase-let* ((`(,acc2 . ,events) (acapella-transport--parse-sse-events (acapella-transport-sse-acc handle) chunk)))
         (setf (acapella-transport-sse-acc handle) acc2)
         (dolist (ev events)
           (let* ((id (cdr (assq 'id ev)))
                  (data (cdr (assq 'data ev))))
             (when id (setf (acapella-transport-sse-last-id handle) id))
             (acapella-transport--traffic-log "SSE EVENT %s id=%s len=%s (reopen)"
                                              url (or id "nil") (if data (length data) 0))
             (when data
               (funcall (acapella-transport-sse-on-event handle) (list :id id :data data))))))))
    (set-process-sentinel
     proc
     (lambda (_proc event)
       (acapella-transport--traffic-log "SSE CLOSE %s %s (reopen)" url (string-trim event))
       (let ((finished (string-match-p (rx (or "finished" "done")) event)))
         (when (and (not finished)
                    (acapella-transport-sse-auto-reconnect handle)
                    (< (or (acapella-transport-sse-retries handle) 0) acapella-sse-reconnect-max))
           (run-at-time acapella-sse-reconnect-delay-seconds nil
                        #'acapella-transport--sse-reopen handle)))
       (when (acapella-transport-sse-on-close handle)
         (funcall (acapella-transport-sse-on-close handle) (list :exit event)))
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

(provide 'acapella-transport)

;;; acapella-transport.el ends here
