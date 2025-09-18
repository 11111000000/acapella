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
        (insert "\n")))))

(defun acapella-transport-open-traffic ()
  "Open the transport traffic buffer."
  (interactive)
  (pop-to-buffer (get-buffer-create acapella-traffic-buffer)))

(defun acapella-transport--alist->headers (headers)
  "Convert HEADERS alist ((\"Name\" . \"Value\") ...) to list of strings \"Name: Value\"."
  (mapcar (lambda (kv) (format "%s: %s" (car kv) (cdr kv))) headers))

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
                                                          (if (string= (downcase (car kv)) "authorization")
                                                              "****"
                                                            (cdr kv))))
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
           (let* ((header-region (buffer-substring (point-min) (match-beginning 0)))
                  (lines (split-string header-region "\r?\n" t)))
             (dolist (l lines)
               (when (string-match "\\`\\([^:]+\\):[ \t]*\\(.*\\)\\'" l)
                 (push (cons (match-string 1 l) (match-string 2 l)) hdrs)))))
         (let* ((body (buffer-substring-no-properties (point) (point-max)))
                (resp (list :status code :headers (nreverse hdrs) :body body :error err)))
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
  headers)

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
           (args (append
                  (list "-sS" "-N" "--no-buffer"
                        "-H" "Accept: text/event-stream"
                        "-H" "Content-Type: application/json")
                  (apply #'append (mapcar (lambda (kv) (list "-H" (format "%s: %s" (car kv) (cdr kv))))
                                          headers))
                  (list "-X" "POST" "-d" data url)))
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
                                                            (if (string= (downcase (car kv)) "authorization")
                                                                "****" (cdr kv))))
                                                  headers ", ")
                                       (length data))
      (let ((handle (acapella-transport--make-sse
                     :process proc :buffer buf :acc ""
                     :on-event on-event :on-close on-close
                     :url url :headers headers)))
        (set-process-filter
         proc
         (lambda (_proc chunk)
           (pcase-let* ((`(,acc2 . ,events) (acapella-transport--parse-sse-events (acapella-transport-sse-acc handle) chunk)))
             (setf (acapella-transport-sse-acc handle) acc2)
             (dolist (ev events)
               (let* ((id (cdr (assq 'id ev)))
                      (data (cdr (assq 'data ev))))
                 (acapella-transport--traffic-log "SSE EVENT %s id=%s len=%s"
                                                  url (or id "nil") (if data (length data) 0))
                 (when data
                   (funcall on-event (list :id id :data data))))))))
        (set-process-sentinel
         proc
         (lambda (_proc event)
           (acapella-transport--traffic-log "SSE CLOSE %s %s" url (string-trim event))
           (when on-close
             (funcall on-close (list :exit event)))
           (when (buffer-live-p buf) (kill-buffer buf))))
        handle))))

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
                                                          (if (string= (downcase (car kv)) "authorization")
                                                              "****"
                                                            (cdr kv))))
                                                headers ", "))
    (url-retrieve
     url
     (lambda (status)
       (let ((err (plist-get status :error))
             (code (or (bound-and-true-p url-http-response-status) 0))
             (hdrs '()))
         (goto-char (point-min))
         (when (re-search-forward "\r?\n\r?\n" nil t)
           (let* ((header-region (buffer-substring (point-min) (match-beginning 0)))
                  (lines (split-string header-region "\r?\n" t)))
             (dolist (l lines)
               (when (string-match "\\`\\([^:]+\\):[ \t]*\\(.*\\)\\'" l)
                 (push (cons (match-string 1 l) (match-string 2 l)) hdrs)))))
         (let* ((body (buffer-substring-no-properties (point) (point-max)))
                (resp (list :status code :headers (nreverse hdrs) :body body :error err)))
           (acapella-transport--traffic-log "HTTP RESP %s code=%s len=%d err=%S"
                                            url code (length body) err)
           (funcall on-done resp))
         (kill-buffer (current-buffer))))
     nil t t)))

(provide 'acapella-transport)

;;; acapella-transport.el ends here
