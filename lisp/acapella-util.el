;;; acapella-util.el --- Small utilities for Acapella -*- lexical-binding: t; -*-

;; Author: Acapella
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.2"))
;; Keywords: json, utils
;; URL: https://example.com/acapella

;;; Commentary:
;; Tiny helpers for JSON, alists and headers. Keep functions small and pure.

;;; Code:

(require 'json)
(require 'subr-x)
(eval-when-compile (require 'cl-lib))

(defun acapella-util-jget (obj key)
  "Get string KEY value from OBJ (alist) using string= key comparison."
  (alist-get key obj nil nil #'string=))

(defun acapella-util-jpath (obj path)
  "Get nested PATH from OBJ (alist).
PATH is a list of keys; string keys compared with string=, integer keys index lists.
Return value or nil."
  (let ((cur obj))
    (cl-loop for k in path
             do (setq cur
                      (cond
                       ((and (stringp k) (listp cur))
                        (alist-get k cur nil nil #'string=))
                       ((and (integerp k) (listp cur))
                        (nth k cur))
                       (t nil)))
             while cur)
    cur))

(defun acapella-util-json-encode (obj)
  "Encode OBJ (alist/list) to JSON string."
  (json-encode obj))

(defun acapella-util-json-parse-safe (s)
  "Parse JSON string S to alist safely.
Return alist on success or error-shaped alist:
  ((\"jsonrpc\" . \"2.0\") (\"error\" . ((\"code\" . -32700) (\"message\" . MSG))))."
  (condition-case err
      (json-parse-string s :object-type 'alist :array-type 'list
                         :null-object json-null :false-object json-false)
    (error `(("jsonrpc" . "2.0")
             ("error" . (("code" . -32700)
                         ("message" . ,(error-message-string err))))))))

(defun acapella-util-ci-header-find (headers name)
  "Find header NAME (case-insensitive) in HEADERS alist, return cons or nil."
  (seq-find (lambda (kv) (string= (downcase (car kv)) (downcase name))) headers))

(defun acapella-util-mask-header (name value)
  "Return masked VALUE for header NAME for logging."
  (if (and (stringp name)
           (string-match-p (rx bos (or "authorization" "x-api-key" "proxy-authorization") eos)
                           (downcase name)))
      (concat (substring value 0 (min 8 (length value))) "…")
    value))

(defun acapella-util-require-exec (program)
  "Ensure PROGRAM is available in PATH, or signal user-error."
  (unless (executable-find program)
    (user-error "[Acapella] %s not found. Please install it" program))
  t)

(provide 'acapella-util)

;;; acapella-util.el ends here
