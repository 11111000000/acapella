;;; acapella-transport-test.el --- Tests for transport helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'acapella-transport)

(ert-deftest acapella-transport-reconnect-delay-linear ()
  "Linear backoff should scale with attempt."
  (let ((acapella-sse-reconnect-backoff 'linear)
        (acapella-sse-reconnect-delay-seconds 2))
    (should (= (funcall (symbol-function 'acapella-transport--reconnect-delay) 1) 2))
    (should (= (funcall (symbol-function 'acapella-transport--reconnect-delay) 3) 6))))

(ert-deftest acapella-transport-reconnect-delay-exponential ()
  "Exponential backoff should grow multiplicatively."
  (let ((acapella-sse-reconnect-backoff 'exponential)
        (acapella-sse-reconnect-delay-seconds 2)
        (acapella-sse-reconnect-backoff-factor 2.0))
    ;; attempt 1: factor^(0) = 1
    (should (= (funcall (symbol-function 'acapella-transport--reconnect-delay) 1) 2))
    ;; attempt 2: factor^(1) = 2
    (should (= (funcall (symbol-function 'acapella-transport--reconnect-delay) 2) 4))
    ;; attempt 4: factor^(3) = 8
    (should (= (funcall (symbol-function 'acapella-transport--reconnect-delay) 4) 16))))

(provide 'acapella-transport-test)
;;; acapella-transport-test.el ends here
