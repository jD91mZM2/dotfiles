(use-package company
  :config
  (global-company-mode 1)
  (setq company-idle-delay 0))
(use-package company-auctex
  :after company)
(use-package company-lsp
  :after company)
(use-package company-math
  :after company
  :config
  (setq-default company-backends (cons 'company-math-symbols-unicode (default-value 'company-backends))))

(use-package yasnippet
  :demand t
  :config
  ;; Rebind TAB to M-/
  (define-key yas-minor-mode-map  (kbd "<tab>")       nil)
  (define-key yas-minor-mode-map  (kbd "TAB")         nil)
  (define-key yas-minor-mode-map  (kbd "M-<ret>")     yas-maybe-expand)
  (define-key yas-keymap          [(tab)]             nil)
  (define-key yas-keymap          (kbd "TAB")         nil)
  (define-key yas-keymap          [(shift tab)]       nil)
  (define-key yas-keymap          [backtab]           nil)
  (define-key yas-keymap          (kbd "<ret>")       'yas-next-field-or-maybe-expand)
  (define-key yas-keymap          (kbd "<S-return>")  'yas-prev-field)

  (setq yas-snippet-dirs (list (my/util/relative "../snippets")))
  (yas-global-mode 1))
