;;; acapella-ui.el --- Minimal UI for Acapella (A2A-first) -*- lexical-binding: t; -*-

;; Author: Acapella
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.2"))
;; Keywords: ui, chat, a2a
;; URL: https://example.com/acapella

;;; Commentary:
;; Minimal user-facing commands:
;; - select profile
;; - send one-shot message (message/send)
;; - stream message (message/stream) with incremental output
;; - cancel streaming
;; - traffic buffer viewer

;;; Code:

(require 'subr-x)
(require 'json)
(require 'acapella-util)
(require 'acapella-config)
(require 'acapella-proto-a2a)
(require 'acapella-transport)

(defgroup acapella-ui nil
  "UI for Acapella."
  :group 'acapella)

(defcustom acapella-chat-buffer "*Acapella Chat*"
  "Name of chat buffer."
  :type 'string
  :group 'acapella-ui)

(defvar acapella--active-sse nil
  "Current SSE handle for streaming, or nil.")

(defvar acapella--last-stream-event nil
  "Last decoded SSE event object from stream/resubscribe (alist).")

(defun acapella-ui--ensure-profile ()
  "Return current profile or ask user to select."
  (or (acapella-current-profile)
      (progn (acapella-select-profile)
             (acapella-current-profile))))

(defun acapella-ui--append (text &optional face)
  "Append TEXT to chat buffer with optional FACE."
  (let ((buf (get-buffer-create acapella-chat-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (if face (propertize text 'face face) text))))
    (unless (get-buffer-window buf) (display-buffer buf))))

(defun acapella-ui--append-line (text &optional face)
  "Append TEXT and newline to chat buffer."
  (acapella-ui--append (concat text "\n") face))

(defun acapella-ui--extract-message-text (obj)
  "Extract text from a JSON-RPC response OBJ (alist) for Message or Task artifacts."
  (let ((result (acapella-util-jget obj "result")))
    (cond
     ;; message result
     ((and result (string= (acapella-util-jget result "kind") "message"))
      (let* ((parts (acapella-util-jget result "parts"))
             (txt (mapconcat (lambda (p) (or (acapella-util-jget p "text") ""))
                             parts "")))
        txt))
     ;; task with artifacts (take text from first artifact's parts)
     ((and result (string= (acapella-util-jget result "kind") "task"))
      (let* ((arts (acapella-util-jget result "artifacts"))
             (first (car arts))
             (parts (and first (acapella-util-jget first "parts")))
             (txt (when parts
                    (mapconcat (lambda (p) (or (acapella-util-jget p "text") ""))
                               parts ""))))
        txt))
     (t nil))))

;; Small, pure helpers for stream events --------------------------------------

(defun acapella-ui--event-text-from-result (result)
  "Return concatenated text from RESULT alist (message|artifact-update), or nil."
  (let ((kind (acapella-util-jget result "kind")))
    (cond
     ((string= kind "message")
      (let* ((parts (acapella-util-jget result "parts")))
        (when parts
          (mapconcat (lambda (p) (or (acapella-util-jget p "text") "")) parts ""))))
     ((string= kind "artifact-update")
      (let* ((art (acapella-util-jget result "artifact"))
             (parts (and art (acapella-util-jget art "parts"))))
        (when parts
          (mapconcat (lambda (p) (or (acapella-util-jget p "text") "")) parts ""))))
     (t nil))))

(defun acapella-ui--event-terminal-state (result)
  "Return marker string for RESULT status-update.
Terminal states return their name when `final' is true.
Additionally, show \"auth-required\" and \"input-required\" markers even if not final."
  (when (string= (acapella-util-jget result "kind") "status-update")
    (let* ((status (acapella-util-jget result "status"))
           (state  (and status (acapella-util-jget status "state")))
           (final  (acapella-util-jget result "final")))
      (cond
       ((and final (member state '("completed" "canceled" "failed" "rejected"))) state)
       ((string= state "auth-required") "auth-required")
       ((string= state "input-required") "input-required")
       (t nil)))))

(defun acapella-ui--on-stream-event (obj)
  "Common handler: extract text from OBJ and append; append marker on terminal state."
  (setq acapella--last-stream-event obj)
  (let* ((result (and obj (acapella-util-jget obj "result")))
         (txt    (and result (acapella-ui--event-text-from-result result)))
         (term   (and result (acapella-ui--event-terminal-state result))))
    (when (and txt (not (string-empty-p txt)))
      (acapella-ui--append txt))
    (when term
      (acapella-ui--append-line (format " [%s]" term) 'shadow)
      (when (string= term "auth-required")
        (acapella-ui--append-line
         "  hint: configure auth-source or check Agent Card securitySchemes" 'shadow)))))

;; Artifact helpers from last stream event -------------------------------------

(defun acapella-ui--artifact-part->source (artifact-name part)
  "Return plist describing PART source for ARTIFACT-NAME.
Keys: :name, :content-type, and either :base64 or :uri."
  (let* ((name artifact-name)
         (ct   (or (acapella-util-jget part "content_type")
                   (acapella-util-jget part "contentType")
                   (acapella-util-jget part "mime")
                   ""))
         (content (acapella-util-jget part "content"))
         (url    (or (acapella-util-jget part "content_url")
                     (acapella-util-jget part "contentUrl"))))
    (cond
     (url (list :name name :content-type ct :uri url))
     (content (list :name name :content-type ct :base64 content))
     (t nil))))

(defun acapella-ui--last-stream-artifact ()
  "Extract last artifact (plist) from `acapella--last-stream-event' or return nil.
Result keys: :name :content-type and either :base64 or :uri."
  (let* ((obj acapella--last-stream-event)
         (res (and obj (acapella-util-jget obj "result")))
         (kind (and res (acapella-util-jget res "kind"))))
    (cond
     ((string= kind "artifact-update")
      (let* ((art   (acapella-util-jget res "artifact"))
             (name  (and art (acapella-util-jget art "name")))
             (parts (and art (acapella-util-jget art "parts"))))
        (catch 'found
          (dolist (p parts)
            (let ((src (acapella-ui--artifact-part->source name p)))
              (when src (throw 'found src))))
          nil)))
     ;; If a task result carried artifacts, try first artifact
     ((string= kind "task")
      (let* ((arts (acapella-util-jget res "artifacts"))
             (first (car arts))
             (name  (and first (or (acapella-util-jget first "name")
                                   (acapella-util-jget first "artifactId"))))
             (parts (and first (acapella-util-jget first "parts"))))
        (catch 'found
          (dolist (p parts)
            (let ((src (acapella-ui--artifact-part->source name p)))
              (when src (throw 'found src))))
          nil)))
     (t nil))))

;;; Interactive commands -------------------------------------------------------

;;;###autoload
(define-minor-mode acapella-mode
  "Toggle Acapella minor mode."
  :global t
  :group 'acapella-ui
  (if acapella-mode
      (message "[Acapella] Mode enabled")
    (when acapella--active-sse
      (ignore-errors (acapella-transport-sse-close acapella--active-sse))
      (setq acapella--active-sse nil))
    (message "[Acapella] Mode disabled")))

;;;###autoload
(defun acapella-open-traffic ()
  "Open transport traffic buffer."
  (interactive)
  (acapella-transport-open-traffic))

;;;###autoload
(defun acapella-send-text (text)
  "Send TEXT to current A2A profile via message/send and show result."
  (interactive (list (read-string "You: ")))
  (let ((profile (acapella-ui--ensure-profile)))
    (acapella-ui--append-line (format "You: %s" text) 'success)
    (acapella-a2a-send
     profile text
     (lambda (resp)
       (let ((err (acapella-util-jget resp "error")))
         (if err
             (acapella-ui--append-line (acapella-ui--format-error err) 'error)
           (let ((txt (acapella-ui--extract-message-text resp)))
             (if (and txt (not (string-empty-p txt)))
                 (acapella-ui--append-line (format "Agent: %s" txt))
               (acapella-ui--append-line "Agent: [no text]" 'shadow)))))) )))

;;;###autoload
(defun acapella-stream-text (text)
  "Stream TEXT to current A2A profile via message/stream."
  (interactive (list (read-string "You (stream): ")))
  (let ((profile (acapella-ui--ensure-profile)))
    (when acapella--active-sse
      (ignore-errors (acapella-transport-sse-close acapella--active-sse))
      (setq acapella--active-sse nil))
    (acapella-ui--append-line (format "You: %s" text) 'success)
    (setq acapella--active-sse
          (acapella-a2a-stream
           profile text
           (lambda (obj)
             (acapella-ui--on-stream-event obj))
           (lambda (info)
             (when (plist-get info :reconnect)
               (acapella-ui--append-line
                (format " [reconnect scheduled: attempt %s/%s, in %ss]"
                        (plist-get info :attempt)
                        (plist-get info :max)
                        (plist-get info :delay))
                'shadow))
             (acapella-ui--append-line ""))))))

;;;###autoload
(defun acapella-stream-cancel ()
  "Cancel current streaming request (if any)."
  (interactive)
  (if (not acapella--active-sse)
      (message "[Acapella] No active stream")
    (acapella-transport-sse-close acapella--active-sse)
    (setq acapella--active-sse nil)
    (acapella-ui--append-line "[Stream canceled]" 'warning)))

;;;###autoload
(defun acapella-get-task (task-id)
  "Fetch TASK-ID via A2A tasks/get and display status."
  (interactive (list (read-string "Task ID: ")))
  (let ((profile (acapella-ui--ensure-profile)))
    (acapella-a2a-get-task
     profile task-id
     (lambda (resp)
       (let ((err (acapella-util-jget resp "error")))
         (if err
             (acapella-ui--append-line (acapella-ui--format-error err) 'error)
           (let* ((result (acapella-util-jget resp "result"))
                  (status (and result (acapella-util-jget result "status")))
                  (state  (and status (acapella-util-jget status "state"))))
             (acapella-ui--append-line
              (format "Task %s state: %s"
                      task-id (or state "[unknown]")) 'shadow))))))))

;;;###autoload
(defun acapella-cancel-task (task-id)
  "Request cancellation of TASK-ID via A2A tasks/cancel."
  (interactive (list (read-string "Task ID to cancel: ")))
  (let ((profile (acapella-ui--ensure-profile)))
    (acapella-a2a-cancel
     profile task-id
     (lambda (resp)
       (let ((err (acapella-util-jget resp "error")))
         (if err
             (acapella-ui--append-line (acapella-ui--format-error err) 'error)
           (acapella-ui--append-line (format "Cancel requested for %s" task-id) 'warning)))))))

;;;###autoload
(defun acapella-resubscribe-task (task-id)
  "Resubscribe to SSE updates for TASK-ID via A2A tasks/resubscribe."
  (interactive (list (read-string "Task ID (resubscribe): ")))
  (let ((profile (acapella-ui--ensure-profile)))
    (when acapella--active-sse
      (ignore-errors (acapella-transport-sse-close acapella--active-sse))
      (setq acapella--active-sse nil))
    (setq acapella--active-sse
          (acapella-a2a-resubscribe
           profile task-id
           (lambda (obj)
             (acapella-ui--on-stream-event obj))
           (lambda (info)
             (when (plist-get info :reconnect)
               (acapella-ui--append-line
                (format " [reconnect scheduled: attempt %s/%s, in %ss]"
                        (plist-get info :attempt)
                        (plist-get info :max)
                        (plist-get info :delay))
                'shadow))
             (acapella-ui--append-line ""))))))

;;;###autoload
(defun acapella-show-agent-card ()
  "Fetch and display Agent Card for current profile in a temporary buffer."
  (interactive)
  (let ((profile (acapella-ui--ensure-profile)))
    (acapella-a2a-fetch-agent-card
     profile
     (lambda (obj)
       (with-current-buffer (get-buffer-create "*Acapella Agent Card*")
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert (if (alist-get "error" obj nil nil #'string=)
                       (format "Error: %s\n" (alist-get "message" (alist-get "error" obj nil nil #'string=) nil nil #'string=))
                     (json-encode obj)))
           (json-pretty-print (point-min) (point-max))
           (goto-char (point-min))
           (view-mode 1)
           (pop-to-buffer (current-buffer))))))))

;;;###autoload
(defun acapella-show-agent-capabilities ()
  "Show Agent Card capabilities (streaming, pushNotifications) for current profile."
  (interactive)
  (let ((profile (acapella-ui--ensure-profile)))
    (acapella-a2a-fetch-agent-card
     profile
     (lambda (obj)
       (let* ((err (alist-get "error" obj nil nil #'string=))
              (caps (and (not err) (alist-get "capabilities" obj nil nil #'string=)))
              (streaming (and caps (alist-get "streaming" caps nil nil #'string=)))
              (push (and caps (alist-get "pushNotifications" caps nil nil #'string=))))
         (if err
             (message "[Acapella] Capabilities error: %s"
                      (alist-get "message" err nil nil #'string=))
           (message "[Acapella] Capabilities: streaming=%s, pushNotifications=%s"
                    (if streaming "true" "false")
                    (if push "true" "false"))))))))

;;;###autoload
(defun acapella-show-agent-security ()
  "Show Agent Card securitySchemes/security for current profile to help configure auth-source."
  (interactive)
  (let ((profile (acapella-ui--ensure-profile)))
    (acapella-a2a-fetch-agent-card
     profile
     (lambda (obj)
       (let* ((err (alist-get "error" obj nil nil #'string=))
              (schemes (and (not err) (alist-get "securitySchemes" obj nil nil #'string=)))
              (security (and (not err) (alist-get "security" obj nil nil #'string=))))
         (cond
          (err
           (message "[Acapella] Security error: %s"
                    (alist-get "message" err nil nil #'string=)))
          ((or schemes security)
           (with-current-buffer (get-buffer-create "*Acapella Agent Security*")
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert (json-encode `(("securitySchemes" . ,schemes)
                                      ("security" . ,security))))
               (json-pretty-print (point-min) (point-max))
               (goto-char (point-min))
               (view-mode 1)
               (pop-to-buffer (current-buffer)))))
          (t
           (message "[Acapella] No security info found in Agent Card"))))))))

;;;###autoload
(defun acapella-validate-agent-card ()
  "Validate Agent Card for current profile and show result."
  (interactive)
  (let ((profile (acapella-ui--ensure-profile)))
    (acapella-a2a-fetch-agent-card
     profile
     (lambda (obj)
       (let ((err (and (not (alist-get "error" obj nil nil #'string=))
                       (acapella-a2a-validate-agent-card obj))))
         (if (or (alist-get "error" obj nil nil #'string=) err)
             (message "[Acapella] Agent Card invalid: %s"
                      (or (and err (alist-get "message" (alist-get "error" err nil nil #'string=) nil nil #'string=))
                          (alist-get "message" (alist-get "error" obj nil nil #'string=) nil nil #'string=)))
           (message "[Acapella] Agent Card looks valid")))))))

;;;###autoload
(defun acapella-resolve-agent-url ()
  "Resolve JSON-RPC URL via Agent Card and show it."
  (interactive)
  (let ((profile (acapella-ui--ensure-profile)))
    (acapella-a2a-resolve-jsonrpc-url
     profile
     (lambda (url)
       (message "[Acapella] JSON-RPC URL: %s" url)))))

;;;###autoload
(defun acapella-copy-agent-url ()
  "Resolve JSON-RPC URL via Agent Card and copy it to kill-ring."
  (interactive)
  (let ((profile (acapella-ui--ensure-profile)))
    (acapella-a2a-resolve-jsonrpc-url
     profile
     (lambda (url)
       (kill-new url)
       (message "[Acapella] Copied JSON-RPC URL: %s" url)))))

;;;###autoload
(defun acapella-show-authenticated-agent-card ()
  "Fetch and display Authenticated Extended Agent Card via JSON-RPC."
  (interactive)
  (let ((profile (acapella-ui--ensure-profile)))
    (acapella-a2a-get-authenticated-card
     profile
     (lambda (obj)
       (with-current-buffer (get-buffer-create "*Acapella Agent Card (Auth)*")
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert (if (alist-get "error" obj nil nil #'string=)
                       (format "Error: %s\n" (alist-get "message" (alist-get "error" obj nil nil #'string=) nil nil #'string=))
                     (json-encode obj)))
           (json-pretty-print (point-min) (point-max))
           (goto-char (point-min))
           (view-mode 1)
           (pop-to-buffer (current-buffer))))))))

;;;###autoload
(defun acapella-clear-agent-card-cache (&optional current-only)
  "Clear Agent Card cache. With CURRENT-ONLY (interactive prefix), clear only current profile."
  (interactive "P")
  (let ((profile (and current-only (acapella-ui--ensure-profile))))
    (acapella-a2a-clear-agent-card-cache profile)
    (message "[Acapella] Agent Card cache %s"
             (if profile "cleared for current profile" "fully cleared"))))

(defun acapella-ui--format-error (err)
  "Format ERR alist into a user-friendly string."
  (let* ((code (acapella-util-jget err "code"))
         (msg  (acapella-util-jget err "message"))
         (data (acapella-util-jget err "data"))
         (wa   (and data (acapella-util-jget data "www-authenticate"))))
    (concat
     (if (numberp code) (format "Error %s" code) "Error")
     (when (and msg (not (string-empty-p msg))) (format ": %s" msg))
     (when wa (format " (WWW-Authenticate: %s)" wa)))))

;;;###autoload
(defun acapella-toggle-extensions ()
  "Toggle A2A Extensions header for the current profile (prompt for values when enabling)."
  (interactive)
  (let ((profile (acapella-ui--ensure-profile)))
    (let* ((name (cdr (assq 'name profile)))
           (ext  (cdr (assq 'extensions profile)))
           (new-ext
            (if ext
                nil
              (let ((s (read-string "Extensions (comma-separated URNs): ")))
                (when (string-empty-p s) nil)
                (mapcar #'string-trim (split-string s "," t))))))
      (setq acapella-profiles
            (mapcar (lambda (p)
                      (if (string= (cdr (assq 'name p)) name)
                          (let ((p2 (copy-sequence p)))
                            (setq p2 (assq-delete-all 'extensions p2))
                            (when new-ext (push (cons 'extensions new-ext) p2))
                            p2)
                        p))
                    acapella-profiles))
      (message "[Acapella] Extensions %s for profile %s"
               (if new-ext "enabled" "disabled") name))))

;;;###autoload
(defun acapella-list-tasks ()
  "Call A2A tasks/list and show the result in a temporary buffer."
  (interactive)
  (let ((profile (acapella-ui--ensure-profile)))
    (acapella-a2a-list-tasks
     profile nil
     (lambda (obj)
       (with-current-buffer (get-buffer-create "*Acapella Tasks*")
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert (json-encode obj))
           (json-pretty-print (point-min) (point-max))
           (goto-char (point-min))
           (view-mode 1)
           (pop-to-buffer (current-buffer))))))))

(defun acapella-ui--open-base64-artifact (name content-type base64)
  "Decode BASE64 and show it in a temporary buffer named after NAME.
If CONTENT-TYPE starts with text/ or is application/json, insert decoded text.
Otherwise insert raw bytes; this is a minimal safe preview."
  (let* ((bytes (ignore-errors (base64-decode-string base64)))
         (buf (get-buffer-create (format "*Acapella Artifact: %s*" (or name "unnamed")))))
    (unless (stringp base64)
      (user-error "[Acapella] Invalid base64 artifact"))
    (unless bytes
      (user-error "[Acapella] Failed to decode base64 artifact"))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (and content-type
                 (string-match-p "\\`\\(text/\\|application/json\\)" (downcase content-type)))
            ;; Try to insert as text (utf-8)
            (insert (decode-coding-string bytes 'utf-8 t))
          ;; Fallback: insert raw
          (insert bytes))
        (goto-char (point-min))
        (view-mode 1)
        (pop-to-buffer (current-buffer))))))

;;;###autoload
(defun acapella-open-artifact-url (url)
  "Check URL against whitelist and inform user; does not download content (safety-first).
This is a placeholder for future safe fetching with size limits and mode-specific viewing."
  (interactive (list (read-string "Artifact URL: ")))
  (if (not (acapella-domain-allowed-p url))
      (message "[Acapella] Domain not allowed by whitelist: %s" url)
    (message "[Acapella] Domain allowed for download: %s (fetching disabled in MVP)" url)))

(defun acapella-ui--open-bytes (name content-type bytes)
  "Open BYTES in a temporary buffer with NAME and CONTENT-TYPE."
  (let* ((buf (get-buffer-create (format "*Acapella Artifact: %s*" (or name "download")))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (acapella--text-mime-p content-type)
            (insert (decode-coding-string bytes 'utf-8 t))
          (insert bytes))
        (goto-char (point-min))
        (view-mode 1)
        (pop-to-buffer (current-buffer))))))

;;;###autoload
(defun acapella-download-artifact-url (url)
  "Safely download artifact from URL if allowed and under size limits.
Respects `acapella-artifact-download-enabled', `acapella-artifact-allowed-domains'
and `acapella-artifact-max-bytes'. Uses Authorization headers from profile (if any)."
  (interactive (list (read-string "Artifact URL: ")))
  (cond
   ((not acapella-artifact-download-enabled)
    (message "[Acapella] Artifact downloading disabled by default (see acapella-artifact-download-enabled)"))
   ((not (acapella-domain-allowed-p url))
    (message "[Acapella] Domain not allowed by whitelist: %s" url))
   (t
    (let* ((profile (acapella-ui--ensure-profile))
           (headers (acapella--headers-with-auth profile)))
      (acapella-transport-http-download
       url headers acapella-artifact-max-bytes
       (lambda (resp)
         (let ((status (plist-get resp :status))
               (ctype  (plist-get resp :content-type))
               (body   (plist-get resp :body)))
           (cond
            ((plist-get resp :too-large)
             (message "[Acapella] Artifact exceeds max size (%d bytes): %s"
                      acapella-artifact-max-bytes url))
            ((and status (>= status 200) (< status 300) (stringp body))
             (let* ((p (url-generic-parse-url url))
                    (path (and p (url-filename p)))
                    (name (and path (file-name-nondirectory path))))
               (acapella-ui--open-bytes name (or ctype "") body)))
            (t
             (message "[Acapella] Download failed (code=%s) for %s" status url))))))))))

;;;###autoload
(defun acapella-open-last-artifact ()
  "Open artifact from the last stream event, if available.
Handles base64 parts with text/binary preview and content_url via safe download."
  (interactive)
  (let ((src (acapella-ui--last-stream-artifact)))
    (cond
     ((null src)
      (message "[Acapella] No artifact found in the last stream event"))
     ((plist-get src :base64)
      (acapella-ui--open-base64-artifact
       (plist-get src :name) (plist-get src :content-type) (plist-get src :base64)))
     ((plist-get src :uri)
      (acapella-download-artifact-url (plist-get src :uri)))
     (t
      (message "[Acapella] Unsupported artifact part in last event")))))

(provide 'acapella-ui)

;;; acapella-ui.el ends here
