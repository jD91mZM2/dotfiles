;; General language support

(use-package flycheck
  :hook (lsp-ui-mode . flycheck-mode))
(use-package lsp-mode
  :bind ("C-c e" . lsp-extend-selection)
  :commands lsp
  :hook (python-mode . lsp)
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-auto-guess-root t)
  (evil-define-key 'normal lsp-mode-map "gd" 'lsp-find-definition))
(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-max-width 50)
  (lsp-ui-doc-max-height 20))

;; Optional dependencies

(use-package htmlize) ;; org-mode: Export to HTML
(use-package edit-indirect) ;; markdown-mode: Edit code blocks

;; More specific modes

;; Emacs Lisp
(use-package auto-compile
  :hook (emacs-lisp-mode . auto-compile-on-save-mode))

;; LaTeX
(use-package auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :pin gnu)

;; Dockerfile
(use-package dockerfile-mode
  :mode "Dockerfile\\'")

;; Go
(use-package go-mode
  :mode "\\.go\\'"
  :hook (go-mode . lsp)
  :init
  (defun goimports ()
    (interactive)
    (when (derived-mode-p 'go-mode)
      (let ((old-pos (point))
            (old-buffer (current-buffer)))
        (let ((new-content (with-temp-buffer
                             (insert-buffer-substring old-buffer)
                             (when (eq (shell-command-on-region (buffer-end 0) (buffer-end 1)
                                                                "goimports" (current-buffer) t
                                                                "GoImports Errors" t)
                                       0)
                               (buffer-string)))))
          (when new-content
            (delete-region (buffer-end 0) (buffer-end 1))
            (insert new-content)
            (goto-char old-pos)))))))

;; JSON
(use-package json-mode
  :mode "\\.json\\'")

;; Markdown
(use-package markdown-mode
  :hook (markdown-mode . flycheck-mode)
  :mode "\\.md\\'"
  :custom
  (markdown-header-scaling t))

;; Nasm
(use-package nasm-mode
  :hook (asm-mode . nasm-mode))

;; Nix
(use-package nix-mode
  :hook (nix-mode . lsp)
  :mode "\\.nix\\'"
  :config
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("bash" "-c" "env RUST_LOG=trace rnix-lsp 2> /tmp/nix-lsp.log"))
                    :major-modes '(nix-mode)
                    :server-id 'nix))
  (setq nix-mode-use-smie t)
  (define-key nix-mode-map (kbd "C-M-x") (lambda (beg end)
                                           (interactive "r")
                                           (shell-command-on-region beg end "nix-instantiate --eval -")))
  (define-key nix-mode-map (kbd "C-c m") (lambda ()
                                           (interactive)
                                           (let ((original-notify Man-notify-method))
                                             (setq Man-notify-method 'pushy)
                                             (man "configuration.nix")
                                             (setq Man-notify-method original-notify)))))

;; Org
(use-package org
  :mode ("\\.org\\'". org-mode)
  :commands org-mode
  :custom ((org-startup-indented t)
           (org-startup-folded nil)))

;; Python
(use-package python-mode
  :mode "\\.py\\'"
  :hook (python-mode . lsp))

;; Rust
(use-package rustic
  :hook (rustic-mode . lsp)
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  (sp-local-pair 'rustic-mode "<" ">"))
(use-package rust-playground
  :after rustic-mode
  :commands (rust-playground rust-playground-mode))

;; Common Lisp
(use-package slime-company
  :commands slime-company)
(use-package slime
  :mode "\\.lisp\\'"
  :commands slime
  :config
  (setq inferior-lisp-program "sbcl --noinform")
  (slime-setup '(slime-fancy slime-company)))

;; YAML
(use-package yaml-mode
  :mode "\\.yml\\'")
