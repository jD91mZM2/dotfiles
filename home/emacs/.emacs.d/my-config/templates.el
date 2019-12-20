(auto-insert-mode 1)

(setq auto-insert-directory (my/util/relative "templates"))
(setq auto-insert-query nil)

;; Reset auto-insert
(setq auto-insert-alist nil)

;; Associate each file with a regex that ends with that string
(dolist (file (directory-files (my/util/relative "templates") nil nil t))
  (unless (or (string= file ".") (string= file ".."))
    (define-auto-insert (concat (regexp-quote file) "\\'") file)))
