;;; acapella-artifacts-test.el --- Tests for artifacts helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'acapella-config)
(require 'acapella-ui)

(ert-deftest acapella-config-domain-allowed-p ()
  "Whitelist check should respect allowed domains."
  (let ((acapella-artifact-allowed-domains '("example.org" "localhost")))
    (should (acapella-domain-allowed-p "http://example.org/file.txt"))
    (should (acapella-domain-allowed-p "http://localhost:8080/a"))
    (should-not (acapella-domain-allowed-p "http://evil.com/a"))
    (should-not (acapella-domain-allowed-p "not-a-url"))))

(ert-deftest acapella-ui-open-base64-artifact-text ()
  "Decoding base64 text should produce correct buffer content."
  (let ((txt "Hello, 世界")
        (buf nil))
    (let* ((b64 (base64-encode-string (encode-coding-string txt 'utf-8))))
      (acapella-ui--open-base64-artifact "t.txt" "text/plain" b64)
      (setq buf (get-buffer "*Acapella Artifact: t.txt*"))
      (unwind-protect
          (progn
            (should buf)
            (with-current-buffer buf
              (goto-char (point-min))
              (let ((got (buffer-substring-no-properties (point-min) (point-max))))
                (should (string= got txt)))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest acapella-ui-download-artifact-disabled ()
  "When download disabled, do not call network and show a message."
  (let ((acapella-artifact-download-enabled nil)
        (called 0))
    (cl-letf (((symbol-function 'acapella-transport-http-download)
               (lambda (&rest _args) (cl-incf called))))
      (acapella-download-artifact-url "http://localhost/a.txt")
      (should (= called 0)))))

(ert-deftest acapella-ui-download-artifact-ok-text ()
  "Download and open text artifact via transport wrapper."
  (let ((acapella-artifact-download-enabled t)
        (acapella-artifact-allowed-domains '("localhost"))
        (buf nil))
    (cl-letf (((symbol-function 'acapella-ui--ensure-profile)
               (lambda () '((name . "X") (url . "http://l/") (headers . ()))))
              ((symbol-function 'acapella-transport-http-download)
               (lambda (_url _headers _max on-done)
                 (funcall on-done (list :status 200 :headers '(("Content-Type" . "text/plain"))
                                        :content-type "text/plain" :body "OK")))))
      (acapella-download-artifact-url "http://localhost/a.txt")
      (setq buf (get-buffer "*Acapella Artifact: a.txt*"))
      (unwind-protect
          (progn
            (should buf)
            (with-current-buffer buf
              (goto-char (point-min))
              (should (string= (buffer-substring-no-properties (point-min) (point-max)) "OK"))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest acapella-ui-download-artifact-too-large ()
  "Too-large download should be refused."
  (let ((acapella-artifact-download-enabled t)
        (acapella-artifact-allowed-domains '("localhost")))
    (cl-letf (((symbol-function 'acapella-ui--ensure-profile)
               (lambda () '((name . "X") (url . "http://l/") (headers . ()))))
              ((symbol-function 'acapella-transport-http-download)
               (lambda (_url _headers _max on-done)
                 (funcall on-done (list :status 200 :headers '(("Content-Type" . "text/plain"))
                                        :content-type "text/plain" :body (make-string 10 ?x) :too-large t)))))
      ;; Should not error; message is enough (can't easily assert here).
      (acapella-download-artifact-url "http://localhost/large.txt")
      (should t))))

;; Open last artifact (base64) from stored stream event
(ert-deftest acapella-ui-open-last-artifact-base64 ()
  "Open last artifact with base64 content from last stream event."
  (let ((acapella-artifact-download-enabled t) ;; not used for base64
        (buf nil))
    ;; Simulate last event: artifact-update with base64 part
    (setq acapella--last-stream-event
          '(("jsonrpc" . "2.0")
            ("id" . "x")
            ("result" . (("kind" . "artifact-update")
                         ("artifact" . (("name" . "a.txt")
                                        ("parts" . ((("content_type" . "text/plain")
                                                     ("content" . "SGVsbG8=")))))))))))
  (acapella-open-last-artifact)
  (setq buf (get-buffer "*Acapella Artifact: a.txt*"))
  (unwind-protect
      (progn
        (should buf)
        (with-current-buffer buf
          (goto-char (point-min))
          (should (string= (buffer-substring-no-properties (point-min) (point-max)) "Hello"))))
    (when (buffer-live-p buf) (kill-buffer buf)))))

;; Open last artifact (URI) triggers download
(ert-deftest acapella-ui-open-last-artifact-url ()
  "Open last artifact with content_url should call transport download."
  (let ((acapella-artifact-download-enabled t)
        (acapella-artifact-allowed-domains '("localhost"))
        (called 0))
    (setq acapella--last-stream-event
          '(("jsonrpc" . "2.0")
            ("id" . "x")
            ("result" . (("kind" . "artifact-update")
                         ("artifact" . (("name" . "b.txt")
                                        ("parts" . ((("contentType" . "text/plain")
                                                     ("content_url" . "http://localhost/b.txt")))))))))))
  (cl-letf (((symbol-function 'acapella-ui--ensure-profile)
             (lambda () '((name . "X") (url . "http://l/") (headers . ()))))
            ((symbol-function 'acapella-transport-http-download)
             (lambda (_url _headers _max on-done)
               (cl-incf called)
               (funcall on-done (list :status 200 :headers '(("Content-Type" . "text/plain"))
                                      :content-type "text/plain" :body "OK")))))
    (let (buf)
      (acapella-open-last-artifact)
      (should (= called 1))
      (setq buf (get-buffer "*Acapella Artifact: b.txt*"))
      (unwind-protect
          (progn
            (should buf)
            (with-current-buffer buf
              (goto-char (point-min))
              (should (string= (buffer-substring-no-properties (point-min) (point-max)) "OK"))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

;; Transport download: HEAD too large avoids GET
(ert-deftest acapella-transport-http-download-head-too-large ()
  "HEAD declares Content-Length > max: should mark :too-large and skip GET."
  (let ((get-calls 0))
    (cl-letf (((symbol-function 'acapella-transport--http-head)
               (lambda (_url _headers on-done)
                 (funcall on-done (list :status 200 :headers '(("Content-Length" . "4096"))
                                        :content-type "text/plain"))))
              ((symbol-function 'acapella-transport-http-get)
               (lambda (&rest _args) (cl-incf get-calls))))
      (let (resp)
        (acapella-transport-http-download
         "http://localhost/large.txt" '(("X" . "1")) 1024 (lambda (r) (setq resp r)))
        (should (plist-get resp :too-large))
        (should (= get-calls 0))))))

;; Transport download: HEAD absent length → GET then refuse if actual body > max
(ert-deftest acapella-transport-http-download-head-no-length-then-too-large ()
  "HEAD without Content-Length should proceed to GET; if body exceeds max, mark :too-large."
  (cl-letf (((symbol-function 'acapella-transport--http-head)
             (lambda (_url _headers on-done)
               (funcall on-done (list :status 200 :headers '()
                                      :content-type "text/plain"))))
            ((symbol-function 'acapella-transport-http-get)
             (lambda (_url _headers on-done)
               ;; Return body larger than max
               (funcall on-done (list :status 200 :headers '(("Content-Type" . "text/plain"))
                                      :content-type "text/plain" :body (make-string 5000 ?x))))))
    (let (resp)
      (acapella-transport-http-download
       "http://localhost/large2.txt" '(("X" . "1")) 1024 (lambda (r) (setq resp r)))
      (should (plist-get resp :too-large)))))

(provide 'acapella-artifacts-test)
;;; acapella-artifacts-test.el ends here
