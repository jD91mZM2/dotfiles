(use-package company
  :commands (company-select-next company-select-previous)
  :config
  (global-company-mode 1)
  (setq company-idle-delay 0)

  ;; <tab>    = Tab key when using graphical emacs
  ;; TAB      = Tab key when using C-i or terminal emacs
  (define-key company-mode-map (kbd "M-<tab>") 'company-indent-or-complete-common)
  (define-key company-mode-map (kbd "M-TAB") 'company-indent-or-complete-common)
  ;; <return> = Return key when using graphical emacs
  ;; RET      = Return key when using C-m or terminal emacs
  (define-key company-active-map (kbd "C-<return>") 'company-complete-selection)
  (define-key company-active-map (kbd "C-RET") 'company-complete-selection)
  (define-key company-active-map (kbd "C-n") 'company-select-next-if-tooltip-visible-or-complete-selection)
  (define-key company-active-map (kbd "C-p") 'company-select-prev))
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
  (setq yas-snippet-dirs (list (my/util/relative "../snippets")))
  (yas-global-mode 1))
