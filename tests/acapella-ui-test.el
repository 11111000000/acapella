;;; acapella-ui-test.el --- Tests for UI helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'acapella-ui)

(ert-deftest acapella-ui-extract-message-text-from-message-result ()
  "Extract concatenated text from Message result parts."
  (let* ((resp '(("jsonrpc" . "2.0")
                 ("id" . "1")
                 ("result" . (("kind" . "message")
                              ("parts" . ((("kind" . "text") ("text" . "Hello"))
                                          (("kind" . "text") ("text" . " World"))))))))
         (txt (funcall (symbol-function 'acapella-ui--extract-message-text) resp)))
    (should (string= txt "Hello World"))))

(ert-deftest acapella-ui-extract-message-text-from-task-artifacts ()
  "Extract text from Task result's first artifact parts."
  (let* ((resp '(("jsonrpc" . "2.0")
                 ("id" . "2")
                 ("result" . (("kind" . "task")
                              ("artifacts" . ((("artifactId" . "a1")
                                               ("parts" . ((("kind" . "text") ("text" . "Alpha")))))
                                              (("artifactId" . "a2")
                                               ("parts" . ((("kind" . "text") ("text" . "Beta")))))))))))
         (txt (funcall (symbol-function 'acapella-ui--extract-message-text) resp)))
    (should (string= txt "Alpha"))))


(ert-deftest acapella-ui-extract-message-text-not-found ()
  "Return nil when result kind is neither message nor task."
  (let* ((resp '(("jsonrpc" . "2.0")
                 ("id" . "3")
                 ("result" . (("kind" . "status-update")
                              ("status" . (("state" . "working")))))))
         (txt (funcall (symbol-function 'acapella-ui--extract-message-text) resp)))
    (should (null txt))))

(ert-deftest acapella-ui-stream-status-marker-completed ()
  "on-stream-event should append a [completed] marker for final status."
  (let ((buf (get-buffer-create acapella-chat-buffer)))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (let ((inhibit-read-only t)) (erase-buffer)))
          (let ((obj '(("jsonrpc" . "2.0")
                       ("id" . "x1")
                       ("result" . (("kind" . "status-update")
                                    ("status" . (("state" . "completed")))
                                    ("final" . t))))))
            (funcall (symbol-function 'acapella-ui--on-stream-event) obj))
          (with-current-buffer buf
            (goto-char (point-min))
            (let ((s (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "\\[completed\\]" s)))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest acapella-ui-stream-status-marker-auth-and-input ()
  "on-stream-event should append markers for auth-required and input-required.
For auth-required also add a hint line."
  (let ((buf (get-buffer-create acapella-chat-buffer)))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (let ((inhibit-read-only t)) (erase-buffer)))
          ;; auth-required
          (let ((obj1 '(("jsonrpc" . "2.0")
                        ("id" . "x2")
                        ("result" . (("kind" . "status-update")
                                     ("status" . (("state" . "auth-required")))
                                     ("final" . json-false))))))
            (funcall (symbol-function 'acapella-ui--on-stream-event) obj1))
          ;; input-required
          (let ((obj2 '(("jsonrpc" . "2.0")
                        ("id" . "x3")
                        ("result" . (("kind" . "status-update")
                                     ("status" . (("state" . "input-required")))
                                     ("final" . json-false))))))
            (funcall (symbol-function 'acapella-ui--on-stream-event) obj2))
          (with-current-buffer buf
            (goto-char (point-min))
            (let ((s (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "\\[auth-required\\]" s))
              (should (string-match-p "configure auth-source" s))
              (should (string-match-p "\\[input-required\\]" s)))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(provide 'acapella-ui-test)
;;; acapella-ui-test.el ends here
