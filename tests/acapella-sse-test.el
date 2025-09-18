;;; acapella-sse-test.el --- Tests for SSE parsing -*- lexical-binding: t; -*-

(require 'ert)
(require 'acapella-transport)

(ert-deftest acapella-sse-parse-single-event ()
  "Parse a simple single SSE event with id and data."
  (pcase-let* ((acc "")
               (`(,acc2 . ,events)
                (acapella-transport--parse-sse-events acc "id: 42\ndata: hello\n\n")))
    (should (string= acc2 ""))
    (should (= (length events) 1))
    (let* ((ev (car events)))
      (should (equal (cdr (assq 'id ev)) "42"))
      (should (equal (cdr (assq 'data ev)) "hello")))))

(ert-deftest acapella-sse-parse-multiline-data ()
  "Parse data split across multiple lines and joined by newline."
  (pcase-let* ((acc "")
               (`(,acc2 . ,events)
                (acapella-transport--parse-sse-events acc "data: hello\ndata: world\n\n")))
    (should (string= acc2 ""))
    (should (= (length events) 1))
    (should (equal (cdr (assq 'data (car events))) "hello\nworld"))))

(ert-deftest acapella-sse-parse-chunk-boundary ()
  "Parse an event arriving in two chunks (simulate streaming)."
  (let* ((acc "")
         step1
         step2
         events1
         events2)
    (setq step1 (acapella-transport--parse-sse-events acc "id: 1\ndata: hel"))
    (setq acc (car step1))
    (setq events1 (cdr step1))
    (should (stringp acc))
    (should (= (length events1) 0)) ;; no full event yet

    (setq step2 (acapella-transport--parse-sse-events acc "lo\n\nid: 2\ndata: next\n\n"))
    (setq acc (car step2))
    (setq events2 (cdr step2))
    (should (string= acc ""))
    (should (= (length events2) 2))
    (let ((ev1 (nth 0 events2))
          (ev2 (nth 1 events2)))
      (should (equal (cdr (assq 'id ev1)) "1"))
      (should (equal (cdr (assq 'data ev1)) "hello"))
      (should (equal (cdr (assq 'id ev2)) "2"))
      (should (equal (cdr (assq 'data ev2)) "next")))))

(ert-deftest acapella-sse-parse-ignore-non-data-lines ()
  "Ensure comment/retry/event lines are ignored without breaking parsing."
  (pcase-let* ((acc "")
               (`(,acc2 . ,events)
                (acapella-transport--parse-sse-events
                 acc ":comment\nretry: 5000\nevent: foo\ndata: ok\n\n")))
    (should (string= acc2 ""))
    (should (= (length events) 1))
    (should (equal (cdr (assq 'data (car events))) "ok"))))

(ert-deftest acapella-sse-parse-crlf ()
  "Parse event separated by CRLF-CRLF and with mixed line endings."
  (pcase-let* ((acc "")
               (`(,acc2 . ,events)
                (acapella-transport--parse-sse-events acc "id: 99\r\ndata: hello\r\n\r\n")))
    (should (string= acc2 ""))
    (should (= (length events) 1))
    (let ((ev (car events)))
      (should (equal (cdr (assq 'id ev)) "99"))
      (should (equal (cdr (assq 'data ev)) "hello")))))

(ert-deftest acapella-sse-parse-no-data-lines ()
  "Event without data lines yields event with nil data (ignored by higher layers)."
  (pcase-let* ((acc "")
               (`(,acc2 . ,events)
                (acapella-transport--parse-sse-events acc "id: 7\r\n\r\n")))
    (should (string= acc2 ""))
    (should (= (length events) 1))
    (let ((ev (car events)))
      (should (equal (cdr (assq 'id ev)) "7"))
      (should (null (cdr (assq 'data ev)))))))

(provide 'acapella-sse-test)
;;; acapella-sse-test.el ends here
