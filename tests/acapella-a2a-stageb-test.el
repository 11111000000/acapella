;;; acapella-a2a-stageb-test.el --- Stage B tests for A2A -*- lexical-binding: t; -*-

(require 'ert)
(require 'acapella-proto-a2a)

(ert-deftest acapella-a2a-normalize-jsonrpc-error-basic ()
  "Known codes map to friendlier messages and keep original text."
  (let* ((err '(("code" . -32002) ("message" . "token missing"))))
    (let* ((norm (acapella-a2a--normalize-jsonrpc-error err))
           (msg (alist-get "message" norm nil nil #'string=)))
      (should (string-match-p "Unauthorized" msg))
      (should (string-match-p "token missing" msg)))))

(ert-deftest acapella-a2a-normalize-http-error ()
  "Negative HTTP status codes are mapped to HTTP-friendly message."
  (let* ((obj '(("jsonrpc" . "2.0") ("error" . (("code" . -401) ("message" . "HTTP error 401"))))))
    (let* ((norm (acapella-a2a--normalize-jsonrpc obj))
           (msg (alist-get "message" (alist-get "error" norm nil nil #'string=) nil nil #'string=)))
      (should (string-match-p "HTTP error 401" msg)))))

(ert-deftest acapella-a2a-select-jsonrpc-url-additional ()
  "When preferredTransport not JSONRPC, pick JSONRPC from additionalInterfaces."
  (let* ((card '(("preferredTransport" . "SSE")
                 ("url" . "http://main.example/")
                 ("additionalInterfaces" .
                  ((("transport" . "gRPC") ("url" . "grpc://host"))
                   (("transport" . "JSONRPC") ("url" . "http://jsonrpc.example/rpc")))))))
    (should (equal (acapella-a2a-select-jsonrpc-url card "http://fallback/") "http://jsonrpc.example/rpc"))))

(ert-deftest acapella-a2a-agent-card-cache-clear ()
  "Clear Agent Card cache for one profile and globally."
  (let* ((profile '((name . "X") (url . "http://host/")))
         (card '(("url" . "http://host/") ("preferredTransport" . "JSONRPC"))))
    ;; put and get
    (acapella-a2a--cache-put-card profile card)
    (let ((entry (acapella-a2a--cache-get-card profile)))
      (should (plist-get entry :card)))
    ;; clear by profile
    (acapella-a2a-clear-agent-card-cache profile)
    (should-not (acapella-a2a--cache-get-card profile))
    ;; put again, then clear all
    (acapella-a2a--cache-put-card profile card)
    (acapella-a2a-clear-agent-card-cache)
    (should-not (acapella-a2a--cache-get-card profile))))

(ert-deftest acapella-a2a-non-json-content-type-returns-parse-error ()
  "HTTP 2xx with non-JSON Content-Type should normalize to JSON parse error (-32700)."
  (let* ((profile '((name . "X") (protocol . a2a) (url . "http://host/"))))
    (cl-letf (((symbol-function 'acapella-transport-http-post)
               (lambda (_url _headers _body on-done)
                 (funcall on-done (list :status 200
                                        :headers '(("Content-Type" . "text/plain"))
                                        :body "not-json")))))
      (let (got)
        (acapella-a2a-send profile "hi" (lambda (resp) (setq got resp)))
        (let ((err (alist-get "error" got nil nil #'string=)))
          (should err)
          (should (= (alist-get "code" err nil nil #'string=) -32700))
          (should (string-match-p "Content-Type" (alist-get "message" err nil nil #'string=))))))))

;; Agent Card validator tests (A2A §5.6.4)

(ert-deftest acapella-a2a-agent-card-validate-missing-preferred ()
  "Validator should fail when preferredTransport is missing."
  (let* ((card '(("url" . "https://a.example/a2a/v1"))))
    (let ((err (acapella-a2a-validate-agent-card card)))
      (should err)
      (should (= (alist-get "code" (alist-get "error" err nil nil #'string=) nil nil #'string=) -32006)))))

(ert-deftest acapella-a2a-agent-card-validate-missing-url-for-jsonrpc ()
  "Validator should fail when JSONRPC preferredTransport has no main url."
  (let* ((card '(("preferredTransport" . "JSONRPC"))))
    (let ((err (acapella-a2a-validate-agent-card card)))
      (should err)
      (should (= (alist-get "code" (alist-get "error" err nil nil #'string=) nil nil #'string=) -32006)))))

(ert-deftest acapella-a2a-agent-card-validate-additional-missing-main-entry ()
  "When additionalInterfaces exists, it SHOULD include entry for main url and preferredTransport."
  (let* ((card '(("preferredTransport" . "JSONRPC")
                 ("url" . "https://a.example/a2a/v1")
                 ("additionalInterfaces" .
                  ((("url" . "https://a.example/a2a/grpc") ("transport" . "GRPC")))))))
    (let ((err (acapella-a2a-validate-agent-card card)))
      (should err)
      (should (= (alist-get "code" (alist-get "error" err nil nil #'string=) nil nil #'string=) -32006)))))

(provide 'acapella-a2a-stageb-test)

;;; acapella-a2a-stageb-test.el ends here
