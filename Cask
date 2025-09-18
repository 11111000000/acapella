(source gnu)
(source melpa)

(package "acapella" "0.1.0" "A2A-first Emacs client for agent protocols")

(files "lisp/*.el")

;; Минимальные зависимости. В коде используются только встроенные json/url.
(depends-on "emacs" "28.2")

;; Разработка/тесты (ert-runner необязателен; тесты запускаем через batch).
(development
 (depends-on "package-build")
 (depends-on "ert-runner"))