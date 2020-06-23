(eval-when-compile (require 'use-package))
(require 'cl-lib)

(use-package company
  :commands (global-company-mode)
  :config
  (global-company-mode 1)
  (setq company-idle-delay 0)

  ;; Insane way to exclude company-etags which seems to sometimes ask if I want
  ;; to keep the current list of tag table or something
  (defun my/company-mode-filter-backends (backends)
    (if (listp backends)
        (cl-loop for backend in backends
                 unless (and (symbolp backend) (string-match-p "tags" (symbol-name backend)))
                 collect (my/company-mode-filter-backends backend))
      backends))
  (setq-default company-backends (my/company-mode-filter-backends company-backends)))
(use-package company-auctex
  :after company)
(use-package company-lsp
  :after company)
(use-package company-math
  :after company
  :config
  (add-to-list 'company-backends 'company-math-symbols-unicode))

(use-package yasnippet
  :demand t
  :commands (yas-global-mode)
  :config
  ;; Rebind TAB to M-/
  (define-key yas-minor-mode-map  (kbd "<tab>")  nil)
  (define-key yas-minor-mode-map  (kbd "TAB")    nil)
  (define-key yas-minor-mode-map  (kbd "M-RET")  yas-maybe-expand)
  (define-key yas-keymap          [(tab)]        nil)
  (define-key yas-keymap          (kbd "TAB")    nil)
  (define-key yas-keymap          [(shift tab)]  nil)
  (define-key yas-keymap          [backtab]      nil)
  (define-key yas-keymap          (kbd "RET")    'yas-next-field-or-maybe-expand)
  (define-key yas-keymap          (kbd "S-RET")  'yas-prev-field)

  (setq yas-snippet-dirs (list (my/util/relative "snippets")))
  (yas-global-mode 1))

(provide 'my-completion)
