;;; acapella-config-test.el --- Tests for config/profile -*- lexical-binding: t; -*-

(require 'ert)
(require 'acapella-config)

(ert-deftest acapella-config-current-profile-by-name ()
  "Selecting profile should return the matching alist."
  (let ((acapella-profiles '(((name . "One") (protocol . a2a) (url . "http://a/"))
                             ((name . "Two") (protocol . a2a) (url . "http://b/")))))
    (setq acapella--current-profile-name "Two")
    (let ((p (acapella-current-profile)))
      (should (equal (cdr (assq 'name p)) "Two"))
      (should (equal (cdr (assq 'url p)) "http://b/")))))

(ert-deftest acapella-config-headers-without-auth ()
  "Headers-with-auth returns base headers when auth-source not configured."
  (let* ((profile '((name . "NoAuth") (protocol . a2a) (url . "http://x/")
                    (headers . (("X-Test" . "1"))) (auth-source . nil)))
         (hs (acapella--headers-with-auth profile)))
    (should (assoc "X-Test" hs))
    (should-not (assoc "Authorization" hs))))

(ert-deftest acapella-config-mask-header ()
  "Masking Authorization and X-API-Key hides value."
  (let ((masked-auth (acapella--mask-header "Authorization" "Bearer SECRET-TOKEN-123456"))
        (masked-key  (acapella--mask-header "X-API-Key" "ABCDEFGH12345"))
        (other       (acapella--mask-header "X-Other" "PLAIN")))
    (should (string-match-p "…\\'" masked-auth))
    (should (string-match-p "…\\'" masked-key))
    (should (equal other "PLAIN"))))

(provide 'acapella-config-test)
;;; acapella-config-test.el ends here
