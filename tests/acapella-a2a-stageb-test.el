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

(provide 'acapella-a2a-stageb-test)

;;; acapella-a2a-stageb-test.el ends here
