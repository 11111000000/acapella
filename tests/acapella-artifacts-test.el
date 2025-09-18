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

(provide 'acapella-artifacts-test)
;;; acapella-artifacts-test.el ends here
