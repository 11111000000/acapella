;;; run-tests.el --- Run Acapella tests in batch -*- lexical-binding: t; -*-

(let* ((this-file (or load-file-name (buffer-file-name)))
       (root (expand-file-name ".." (file-name-directory this-file)))
       (lisp (expand-file-name "lisp" root))
       (tests (expand-file-name "tests" root)))
  (add-to-list 'load-path lisp)
  (add-to-list 'load-path tests))

(require 'ert)

;; Load implementation
(require 'acapella-config)
(require 'acapella-transport)

(require 'acapella-proto-a2a)
;; UI not strictly needed for unit tests; load if present
(ignore-errors (require 'acapella-ui))

;; Load tests
(load "acapella-sse-test.el" nil t)
(load "acapella-a2a-test.el" nil t)
(load "acapella-config-test.el" nil t)
(load "acapella-ui-test.el" nil t)
(load "acapella-a2a-stageb-test.el" nil t)
(load "acapella-transport-test.el" nil t)
(load "acapella-artifacts-test.el" nil t)

;; Run all tests
(ert-run-tests-batch-and-exit)
;;; run-tests.el ends here
