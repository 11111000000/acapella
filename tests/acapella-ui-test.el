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

(provide 'acapella-ui-test)
;;; acapella-ui-test.el ends here
