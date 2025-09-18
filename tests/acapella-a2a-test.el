;;; acapella-a2a-test.el --- Tests for A2A adapter -*- lexical-binding: t; -*-

(require 'ert)
(require 'json)
(require 'acapella-proto-a2a)
(require 'acapella-config)

(defun acapella-a2a-test--dummy-profile ()
  '((name . "Test A2A")
    (protocol . a2a)
    (url . "http://localhost:9999/")
    (headers . ())))

(ert-deftest acapella-a2a-jsonrpc-envelope ()
  "Envelope must contain jsonrpc 2.0, method and id."
  (let* ((env (funcall (symbol-function 'acapella-a2a--rpc-envelope)
                       "message/send" '(("x" . 1)))))
    (should (equal (alist-get "jsonrpc" env nil nil #'string=) "2.0"))
    (should (equal (alist-get "method" env nil nil #'string=) "message/send"))
    (should (stringp (alist-get "id" env nil nil #'string=)))
    (should (alist-get "params" env nil nil #'string=))))

(ert-deftest acapella-a2a-normalize-jsonrpc-error-hints ()
  "Normalization adds friendly message and hint for known A2A codes."
  (let* ((err '(("code" . -32002) ("message" . "token missing"))))
    (let* ((norm (acapella-a2a--normalize-jsonrpc-error err))
           (msg (alist-get "message" norm nil nil #'string=))
           (data (alist-get "data" norm nil nil #'string=))
           (hint (and data (alist-get "hint" data nil nil #'string=))))
      (should (string-match-p "Unauthorized" msg))
      (should (string-match-p "token missing" msg))
      (should (stringp hint)
              "Expected a hint string to be present for A2A code -32002")))

  (ert-deftest acapella-a2a-normalize-http-error-hint ()
    "HTTP negative code mapping keeps message and no hint is added."
    (let* ((err '(("code" . -401) ("message" . "Unauthorized"))))
      (let* ((norm (acapella-a2a--normalize-jsonrpc-error err))
             (msg (alist-get "message" norm nil nil #'string=))
             (data (alist-get "data" norm nil nil #'string=)))
        (should (string-match-p "HTTP error 401" msg))
        (should (null data)))))

  (ert-deftest acapella-a2a-text-message-shape ()
    "Text message must be role=user with parts array of text kind."
    (let* ((m (funcall (symbol-function 'acapella-a2a--text-message) "hi" "task-1" "ctx-1"))
           (role (alist-get "role" m nil nil #'string=))
           (parts (alist-get "parts" m nil nil #'string=)))
      (should (equal role "user"))
      (should (listp parts))
      (should (equal (alist-get "kind" (car parts) nil nil #'string=) "text"))
      (should (equal (alist-get "text" (car parts) nil nil #'string=) "hi"))
      (should (equal (alist-get "taskId" m nil nil #'string=) "task-1"))
      (should (equal (alist-get "contextId" m nil nil #'string=) "ctx-1"))))

  (ert-deftest acapella-a2a-send-success-calls-callback-with-parsed-json ()
    "acapella-a2a-send should parse JSON and pass alist to callback on HTTP 200."
    (let* ((profile (acapella-a2a-test--dummy-profile))
           (called nil)
           (result nil))
      (cl-letf (((symbol-function 'acapella-transport-http-post)
                 (lambda (_url _headers body on-done)
                   ;; Ensure JSON-RPC request is well-formed
                   (let* ((obj (json-parse-string body :object-type 'alist))
                          (method (alist-get "method" obj nil nil #'string=)))
                     (should (equal method "message/send")))
                   ;; Call once with proper 200 and body
                   (funcall on-done
                            (list :status 200 :headers nil
                                  :body "{\"jsonrpc\":\"2.0\",\"id\":\"x\",\"result\":{\"kind\":\"message\",\"parts\":[{\"text\":\"Hello World\"}]}}")))))
        (acapella-a2a-send
         profile "hi"
         (lambda (resp)
           (setq called t
                 result resp)))
        (should called)
        (should (equal (alist-get "jsonrpc" result nil nil #'string=) "2.0"))
        (should (equal (alist-get "kind" (alist-get "result" result nil nil #'string=) nil nil #'string=) "message")))))

  (ert-deftest acapella-a2a-stream-calls-on-event-for-sse-data ()
    "acapella-a2a-stream should decode each SSE data JSON and invoke on-event."
    (let* ((profile (acapella-a2a-test--dummy-profile))
           (events '()))
      ;; Mock SSE open to directly feed two events
      (cl-letf (((symbol-function 'acapella-transport-sse-open)
                 (lambda (_url _headers _payload on-event on-close)
                   (funcall on-event (list :id "1" :data "{\"jsonrpc\":\"2.0\",\"id\":\"1\",\"result\":{\"kind\":\"message\",\"parts\":[{\"text\":\"A\"}]}}"))
                   (funcall on-event (list :id "2" :data "{\"jsonrpc\":\"2.0\",\"id\":\"2\",\"result\":{\"kind\":\"status-update\",\"status\":{\"state\":\"completed\"},\"final\":true}}"))
                   (funcall on-close (list :exit "finished"))
                   ;; Return dummy handle
                   'dummy-handle)))
        (let ((handle (acapella-a2a-stream
                       profile "stream this"
                       (lambda (obj) (push obj events))
                       (lambda (_c) (push 'closed events)))))
          (should (eq handle 'dummy-handle))
          (should (= (length events) 3))
          ;; last pushed is 'closed
          (should (eq (car events) 'closed))
          (let* ((ev2 (cadr events))
                 (kind2 (alist-get "kind" (alist-get "result" ev2 nil nil #'string=) nil nil #'string=)))
            (should (equal kind2 "status-update")))))))

  (ert-deftest acapella-a2a-get-task-envelopes-and-callback ()
    "tasks/get must produce proper envelope and call callback with parsed JSON."
    (let* ((profile (acapella-a2a-test--dummy-profile))
           (called nil)
           (got nil))
      (cl-letf (((symbol-function 'acapella-transport-http-post)
                 (lambda (_url _headers body on-done)
                   (let* ((obj (json-parse-string body :object-type 'alist))
                          (m (alist-get "method" obj nil nil #'string=))
                          (params (alist-get "params" obj nil nil #'string=)))
                     (should (equal m "tasks/get"))
                     (should (equal (alist-get "taskId" params nil nil #'string=) "T-1")))
                   (funcall on-done (list :status 200 :headers nil :body "{\"jsonrpc\":\"2.0\",\"id\":\"x\",\"result\":{\"kind\":\"task\",\"status\":{\"state\":\"working\"}}}")))))
        (acapella-a2a-get-task
         profile "T-1"
         (lambda (resp) (setq called t got resp)))
        (should called)
        (should (equal (alist-get "jsonrpc" got nil nil #'string=) "2.0"))
        (should (equal (alist-get "kind" (alist-get "result" got nil nil #'string=) nil nil #'string=) "task")))))

  (ert-deftest acapella-a2a-cancel-envelopes-and-callback ()
    "tasks/cancel must produce proper envelope and call callback with parsed JSON."
    (let* ((profile (acapella-a2a-test--dummy-profile))
           (called nil)
           (got nil))
      (cl-letf (((symbol-function 'acapella-transport-http-post)
                 (lambda (_url _headers body on-done)
                   (let* ((obj (json-parse-string body :object-type 'alist))
                          (m (alist-get "method" obj nil nil #'string=))
                          (params (alist-get "params" obj nil nil #'string=)))
                     (should (equal m "tasks/cancel"))
                     (should (equal (alist-get "taskId" params nil nil #'string=) "T-2")))
                   (funcall on-done (list :status 200 :headers nil :body "{\"jsonrpc\":\"2.0\",\"id\":\"x\",\"result\":{\"ok\":true}}")))))
        (acapella-a2a-cancel
         profile "T-2"
         (lambda (resp) (setq called t got resp)))
        (should called)
        (should (equal (alist-get "jsonrpc" got nil nil #'string=) "2.0"))
        (should (alist-get "result" got nil nil #'string=)))))

  (ert-deftest acapella-a2a-resubscribe-envelopes-and-callbacks ()
    "tasks/resubscribe must POST SSE with proper JSON-RPC and deliver events."
    (let* ((profile (acapella-a2a-test--dummy-profile))
           (seen '()))
      (cl-letf (((symbol-function 'acapella-transport-sse-open)
                 (lambda (_url _headers payload on-event on-close)
                   (let* ((obj (json-parse-string payload :object-type 'alist))
                          (m (alist-get "method" obj nil nil #'string=))
                          (params (alist-get "params" obj nil nil #'string=)))
                     (should (equal m "tasks/resubscribe"))
                     (should (equal (alist-get "taskId" params nil nil #'string=) "T-42")))
                   (funcall on-event (list :id "1" :data "{\"jsonrpc\":\"2.0\",\"id\":\"1\",\"result\":{\"kind\":\"message\",\"parts\":[{\"text\":\"Chunk\"}]}}"))
                   (funcall on-close (list :exit "done"))
                   'handle)))
        (let ((h (acapella-a2a-resubscribe
                  profile "T-42"
                  (lambda (obj) (push obj seen))
                  (lambda (_c) (push 'closed seen)))))
          (should (eq h 'handle))
          (should (= (length seen) 2))
          (should (eq (car seen) 'closed))
          (should (equal (alist-get "kind" (alist-get "result" (cadr seen) nil nil #'string=) nil nil #'string=) "message"))))))

  (ert-deftest acapella-a2a-agent-card-url-selection ()
    "Select JSONRPC URL from Agent Card by preferredTransport and interfaces."
    (let* ((card1 '(("preferredTransport" . "JSONRPC")
                    ("url" . "https://a.example.com/a2a/v1")))
           (card2 '(("preferredTransport" . "GRPC")
                    ("url" . "https://a.example.com/grpc")
                    ("additionalInterfaces" . ((("url" . "https://a.example.com/a2a/v1")
                                                ("transport" . "JSONRPC")))))))
      (should (equal (acapella-a2a-select-jsonrpc-url card1 "FALLBACK") "https://a.example.com/a2a/v1"))
      (should (equal (acapella-a2a-select-jsonrpc-url card2 "FALLBACK") "https://a.example.com/a2a/v1"))
      (should (equal (acapella-a2a-select-jsonrpc-url '(("preferredTransport" . "GRPC")) "FALLBACK") "FALLBACK"))))

  (ert-deftest acapella-a2a-agent-card-fetch-and-resolve ()
    "Fetch Agent Card via GET and resolve JSON-RPC URL (with fallback on error)."
    (let* ((profile (acapella-a2a-test--dummy-profile))
           (called nil)
           (resolved nil))
      (clrhash acapella-a2a--agent-card-cache)
      (cl-letf (((symbol-function 'acapella-transport-http-get)
                 (lambda (_url _headers on-done)
                   ;; Return a minimal card supporting JSONRPC on main url
                   (funcall on-done (list :status 200 :headers nil
                                          :body "{\"url\":\"https://x.example/a2a/v1\",\"preferredTransport\":\"JSONRPC\"}")))))
        (acapella-a2a-resolve-jsonrpc-url
         profile
         (lambda (url) (setq called t resolved url))))
      (should called)
      (should (equal resolved "https://x.example/a2a/v1"))))

  (ert-deftest acapella-a2a-agent-card-resolve-fallback-on-error ()
    "Resolve JSON-RPC URL falls back to profile :url when Agent Card GET fails."
    (let* ((profile (acapella-a2a-test--dummy-profile))
           (called nil)
           (resolved nil))
      (clrhash acapella-a2a--agent-card-cache)
      (cl-letf (((symbol-function 'acapella-transport-http-get)
                 (lambda (_url _headers on-done)
                   ;; Simulate HTTP error (e.g., 500)
                   (funcall on-done (list :status 500 :headers nil :body "Internal Error")))))
        (acapella-a2a-resolve-jsonrpc-url
         profile
         (lambda (url) (setq called t resolved url))))
      (should called)
      (should (equal resolved (cdr (assq 'url (acapella-a2a-test--dummy-profile)))))))

  (ert-deftest acapella-a2a-auth-card-jsonrpc-envelope ()
    "Authenticated Agent Card call uses correct JSON-RPC method."
    (let* ((profile (acapella-a2a-test--dummy-profile))
           (called nil))
      (cl-letf (((symbol-function 'acapella-transport-http-post)
                 (lambda (_url _headers body on-done)
                   (let* ((obj (json-parse-string body :object-type 'alist))
                          (m (alist-get "method" obj nil nil #'string=)))
                     (should (equal m "agent/getAuthenticatedExtendedCard")))
                   (funcall on-done (list :status 200 :headers nil :body "{\"jsonrpc\":\"2.0\",\"id\":\"1\",\"result\":{\"name\":\"X\"}}")))))
        (acapella-a2a-get-authenticated-card
         profile
         (lambda (_resp) (setq called t))))
      (should called)))

  (ert-deftest acapella-a2a-auth-card-callback-success ()
    "Authenticated card success response is parsed and passed to callback."
    (let* ((profile (acapella-a2a-test--dummy-profile))
           (got nil))
      (cl-letf (((symbol-function 'acapella-transport-http-post)
                 (lambda (_url _headers _body on-done)
                   (funcall on-done (list :status 200 :headers nil :body "{\"jsonrpc\":\"2.0\",\"id\":\"1\",\"result\":{\"url\":\"https://x\",\"preferredTransport\":\"JSONRPC\"}}")))))
        (acapella-a2a-get-authenticated-card
         profile
         (lambda (resp) (setq got resp))))
      (should (equal (alist-get "jsonrpc" got nil nil #'string=) "2.0"))
      (should (alist-get "result" got nil nil #'string=))))

  (ert-deftest acapella-a2a-auth-card-http-error ()
    "Authenticated card HTTP error is mapped to JSON-RPC-like error."
    (let* ((profile (acapella-a2a-test--dummy-profile))
           (got nil))
      (cl-letf (((symbol-function 'acapella-transport-http-post)
                 (lambda (_url _headers _body on-done)
                   (funcall on-done (list :status 401 :headers nil :body "Unauthorized")))))
        (acapella-a2a-get-authenticated-card
         profile
         (lambda (resp) (setq got resp))))
      (let ((err (alist-get "error" got nil nil #'string=)))
        (should err)
        (should (= (alist-get "code" err nil nil #'string=) -401)))))

  ;; Extensions header is sent when profile declares extensions
  (ert-deftest acapella-a2a-extensions-header-sent ()
    "Profile 'extensions adds X-A2A-Extensions header."
    (let* ((profile '((name . "Ext")
                      (protocol . a2a)
                      (url . "http://localhost:9999/")
                      (headers . ())
                      (extensions . ("urn:x:ext1" "urn:x:ext2"))))
           (seen nil))
      (cl-letf (((symbol-function 'acapella-transport-http-post)
                 (lambda (_url headers _body on-done)
                   (setq seen (assoc "X-A2A-Extensions" headers))
                   (funcall on-done (list :status 200 :headers nil :body "{\"jsonrpc\":\"2.0\",\"id\":\"1\",\"result\":{\"ok\":true}}")))))
        (acapella-a2a-send profile "hi" (lambda (_r) nil))
        (should seen)
        (should (string-match-p "urn:x:ext1" (cdr seen)))
        (should (string-match-p "urn:x:ext2" (cdr seen))))))

  ;; Push Notification Config RPCs

  (ert-deftest acapella-a2a-push-set-envelope ()
    "tasks/pushNotificationConfig/set sends config and parses result."
    (let* ((profile (acapella-a2a-test--dummy-profile))
           (called nil))
      (cl-letf (((symbol-function 'acapella-transport-http-post)
                 (lambda (_url _headers body on-done)
                   (let* ((obj (json-parse-string body :object-type 'alist))
                          (m (alist-get "method" obj nil nil #'string=))
                          (params (alist-get "params" obj nil nil #'string=)))
                     (should (equal m "tasks/pushNotificationConfig/set"))
                     (should (equal (alist-get "taskId" params nil nil #'string=) "T-77")))
                   (funcall on-done (list :status 200 :headers nil :body "{\"jsonrpc\":\"2.0\",\"id\":\"1\",\"result\":{\"taskId\":\"T-77\"}}")))))
        (acapella-a2a-push-set
         profile '(("taskId" . "T-77") ("url" . "https://client/webhook") ("token" . "X"))
         (lambda (resp)
           (setq called (equal (alist-get "jsonrpc" resp nil nil #'string=) "2.0")))))
      (should called)))

  (ert-deftest acapella-a2a-push-get-envelope ()
    "tasks/pushNotificationConfig/get expects taskId+configId."
    (let* ((profile (acapella-a2a-test--dummy-profile))
           (ok nil))
      (cl-letf (((symbol-function 'acapella-transport-http-post)
                 (lambda (_url _headers body on-done)
                   (let* ((obj (json-parse-string body :object-type 'alist))
                          (m (alist-get "method" obj nil nil #'string=))
                          (params (alist-get "params" obj nil nil #'string=)))
                     (should (equal m "tasks/pushNotificationConfig/get"))
                     (should (equal (alist-get "taskId" params nil nil #'string=) "T-1"))
                     (should (equal (alist-get "configId" params nil nil #'string=) "C-9")))
                   (funcall on-done (list :status 200 :headers nil :body "{\"jsonrpc\":\"2.0\",\"id\":\"1\",\"result\":{\"taskId\":\"T-1\",\"configId\":\"C-9\"}}")))))
        (acapella-a2a-push-get profile "T-1" "C-9" (lambda (_r) (setq ok t))))
      (should ok)))

  (ert-deftest acapella-a2a-push-list-envelope ()
    "tasks/pushNotificationConfig/list expects taskId."
    (let* ((profile (acapella-a2a-test--dummy-profile))
           (ok nil))
      (cl-letf (((symbol-function 'acapella-transport-http-post)
                 (lambda (_url _headers body on-done)
                   (let* ((obj (json-parse-string body :object-type 'alist))
                          (m (alist-get "method" obj nil nil #'string=))
                          (params (alist-get "params" obj nil nil #'string=)))
                     (should (equal m "tasks/pushNotificationConfig/list"))
                     (should (equal (alist-get "taskId" params nil nil #'string=) "T-LST")))
                   (funcall on-done (list :status 200 :headers nil :body "{\"jsonrpc\":\"2.0\",\"id\":\"1\",\"result\":[]}")))))
        (acapella-a2a-push-list profile "T-LST" (lambda (_r) (setq ok t))))
      (should ok)))

  (ert-deftest acapella-a2a-push-delete-envelope ()
    "tasks/pushNotificationConfig/delete expects taskId+configId."
    (let* ((profile (acapella-a2a-test--dummy-profile))
           (ok nil))
      (cl-letf (((symbol-function 'acapella-transport-http-post)
                 (lambda (_url _headers body on-done)
                   (let* ((obj (json-parse-string body :object-type 'alist))
                          (m (alist-get "method" obj nil nil #'string=))
                          (params (alist-get "params" obj nil nil #'string=)))
                     (should (equal m "tasks/pushNotificationConfig/delete"))
                     (should (equal (alist-get "taskId" params nil nil #'string=) "T-DEL"))
                     (should (equal (alist-get "configId" params nil nil #'string=) "C-DEL")))
                   (funcall on-done (list :status 200 :headers nil :body "{\"jsonrpc\":\"2.0\",\"id\":\"1\",\"result\":null}")))))
        (acapella-a2a-push-delete profile "T-DEL" "C-DEL" (lambda (_r) (setq ok t))))
      (should ok)))

  ;; Agent Card cache tests

  (ert-deftest acapella-a2a-agent-card-cache-hit ()
    "Second resolve within TTL should use cached card (single GET call)."
    (let* ((profile (acapella-a2a-test--dummy-profile))
           (calls 0))
      (let ((acapella-agent-card-ttl-seconds 300))
        (cl-letf (((symbol-function 'acapella-transport-http-get)
                   (lambda (_url _headers on-done)
                     (cl-incf calls)
                     (funcall on-done (list :status 200 :headers nil
                                            :body "{\"url\":\"https://cached.example/a2a/v1\",\"preferredTransport\":\"JSONRPC\"}")))))
          (let (u1 u2)
            (acapella-a2a-resolve-jsonrpc-url profile (lambda (u) (setq u1 u)))
            (acapella-a2a-resolve-jsonrpc-url profile (lambda (u) (setq u2 u)))
            (should (equal u1 "https://cached.example/a2a/v1"))
            (should (equal u2 "https://cached.example/a2a/v1"))
            (should (= calls 1)))))))

  (ert-deftest acapella-a2a-agent-card-cache-stale-on-error ()
    "If TTL expired and GET fails, resolver should fall back to stale cached card when present."
    (let* ((profile (acapella-a2a-test--dummy-profile))
           (calls 0))
      (let ((acapella-agent-card-ttl-seconds 0))
        (cl-letf (((symbol-function 'acapella-transport-http-get)
                   (lambda (_url _headers on-done)
                     (cl-incf calls)
                     (if (= calls 1)
                         (funcall on-done (list :status 200 :headers nil
                                                :body "{\"url\":\"https://first.example/a2a/v1\",\"preferredTransport\":\"JSONRPC\"}"))
                       (funcall on-done (list :status 500 :headers nil :body "Internal"))))))
          (let (u1 u2)
            (acapella-a2a-resolve-jsonrpc-url profile (lambda (u) (setq u1 u)))
            (acapella-a2a-resolve-jsonrpc-url profile (lambda (u) (setq u2 u)))
            (should (equal u1 "https://first.example/a2a/v1"))
            (should (equal u2 "https://first.example/a2a/v1"))
            (should (= calls 2)))))))

  (ert-deftest acapella-a2a-auth-card-401-includes-www-authenticate ()
    "Authenticated card error should include WWW-Authenticate header in error.data."
    (let* ((profile (acapella-a2a-test--dummy-profile))
           (got nil))
      (cl-letf (((symbol-function 'acapella-transport-http-post)
                 (lambda (_url _headers _body on-done)
                   (funcall on-done (list :status 401
                                          :headers '(("WWW-Authenticate" . "Bearer realm=\"x\""))
                                          :body "Unauthorized")))))
        (acapella-a2a-get-authenticated-card
         profile
         (lambda (resp) (setq got resp))))
      (let* ((err (alist-get "error" got nil nil #'string=))
             (data (and err (alist-get "data" err nil nil #'string=))))
        (should err)
        (should (= (alist-get "code" err nil nil #'string=) -401))
        (should (string-match-p "Bearer" (alist-get "www-authenticate" data nil nil #'string=))))))

  (ert-deftest acapella-a2a-tasks-list-envelope ()
    "tasks/list must produce proper JSON-RPC method and params."
    (let* ((profile (acapella-a2a-test--dummy-profile))
           (called nil))
      (cl-letf (((symbol-function 'acapella-transport-http-post)
                 (lambda (_url _headers body on-done)
                   (let* ((obj (json-parse-string body :object-type 'alist))
                          (m (alist-get "method" obj nil nil #'string=)))
                     (should (equal m "tasks/list")))
                   (funcall on-done (list :status 200 :headers nil
                                          :body "{\"jsonrpc\":\"2.0\",\"id\":\"1\",\"result\":[]}")))))
        (acapella-a2a-list-tasks profile nil (lambda (_resp) (setq called t))))
      (should called)))

  (provide 'acapella-a2a-test)
;;; acapella-a2a-test.el ends here
