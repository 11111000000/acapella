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

(defun acapella-ui--jget (obj key)
  "Get string KEY value from OBJ (alist) using string= comparison."
  (alist-get key obj nil nil #'string=))

(defun acapella-ui--extract-message-text (obj)
  "Extract text from a JSON-RPC response OBJ (alist) for Message or Task artifacts."
  (let ((result (acapella-ui--jget obj "result")))
    (cond
     ;; message result
     ((and result (string= (acapella-ui--jget result "kind") "message"))
      (let* ((parts (acapella-ui--jget result "parts"))
             (txt (mapconcat (lambda (p) (or (acapella-ui--jget p "text") ""))
                             parts "")))
        txt))
     ;; task with artifacts (take text from first artifact's parts)
     ((and result (string= (acapella-ui--jget result "kind") "task"))
      (let* ((arts (acapella-ui--jget result "artifacts"))
             (first (car arts))
             (parts (and first (acapella-ui--jget first "parts")))
             (txt (when parts
                    (mapconcat (lambda (p) (or (acapella-ui--jget p "text") ""))
                               parts ""))))
        txt))
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
       (let ((err (acapella-ui--jget resp "error")))
         (if err
             (acapella-ui--append-line
              (format "Error: %s" (acapella-ui--jget err "message")) 'error)
           (let ((txt (acapella-ui--extract-message-text resp)))
             (if (and txt (not (string-empty-p txt)))
                 (acapella-ui--append-line (format "Agent: %s" txt))
               (acapella-ui--append-line "Agent: [no text]" 'shadow)))))))))

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
             ;; Try extract and append text incrementally (message or artifact-update)
             (let ((result (acapella-ui--jget obj "result")))
               (cond
                ;; message chunk
                ((and result (string= (acapella-ui--jget result "kind") "message"))
                 (let* ((parts (acapella-ui--jget result "parts"))
                        (txt (mapconcat (lambda (p) (or (acapella-ui--jget p "text") "")) parts "")))
                   (when (and txt (not (string-empty-p txt)))
                     (acapella-ui--append txt))))
                ;; artifact-update chunk
                ((and result (string= (acapella-ui--jget result "kind") "artifact-update"))
                 (let* ((art (acapella-ui--jget result "artifact"))
                        (parts (and art (acapella-ui--jget art "parts")))
                        (txt (when parts (mapconcat (lambda (p) (or (acapella-ui--jget p "text") "")) parts ""))))
                   (when (and txt (not (string-empty-p txt)))
                     (acapella-ui--append txt))))
                ;; status updates: show completed
                ((and result (string= (acapella-ui--jget result "kind") "status-update"))
                 (let* ((status (acapella-ui--jget result "status"))
                        (state (and status (acapella-ui--jget status "state")))
                        (final (acapella-ui--jget result "final")))
                   (when (and final (string= state "completed"))
                     (acapella-ui--append-line ""))))
                (t ;; ignore other events, keep traffic in log
                 ))))
           (lambda (_exit)
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
       (let ((err (acapella-ui--jget resp "error")))
         (if err
             (acapella-ui--append-line
              (format "Error (get-task): %s" (acapella-ui--jget err "message")) 'error)
           (let* ((result (acapella-ui--jget resp "result"))
                  (status (and result (acapella-ui--jget result "status")))
                  (state  (and status (acapella-ui--jget status "state"))))
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
       (let ((err (acapella-ui--jget resp "error")))
         (if err
             (acapella-ui--append-line
              (format "Error (cancel): %s" (acapella-ui--jget err "message")) 'error)
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
             (let ((result (acapella-ui--jget obj "result")))
               (cond
                ((and result (string= (acapella-ui--jget result "kind") "message"))
                 (let* ((parts (acapella-ui--jget result "parts"))
                        (txt (mapconcat (lambda (p) (or (acapella-ui--jget p "text") "")) parts "")))
                   (when (and txt (not (string-empty-p txt)))
                     (acapella-ui--append txt))))
                ((and result (string= (acapella-ui--jget result "kind") "artifact-update"))
                 (let* ((art (acapella-ui--jget result "artifact"))
                        (parts (and art (acapella-ui--jget art "parts")))
                        (txt (when parts (mapconcat (lambda (p) (or (acapella-ui--jget p "text") "")) parts ""))))
                   (when (and txt (not (string-empty-p txt)))
                     (acapella-ui--append txt))))
                ((and result (string= (acapella-ui--jget result "kind") "status-update"))
                 (let* ((status (acapella-ui--jget result "status"))
                        (state (and status (acapella-ui--jget status "state")))
                        (final (acapella-ui--jget result "final")))
                   (when (and final (string= state "completed"))
                     (acapella-ui--append-line ""))))
                (t))))
           (lambda (_exit)
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
(defun acapella-resolve-agent-url ()
  "Resolve JSON-RPC URL via Agent Card and show it."
  (interactive)
  (let ((profile (acapella-ui--ensure-profile)))
    (acapella-a2a-resolve-jsonrpc-url
     profile
     (lambda (url)
       (message "[Acapella] JSON-RPC URL: %s" url)))))

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

(provide 'acapella-ui)

;;; acapella-ui.el ends here
