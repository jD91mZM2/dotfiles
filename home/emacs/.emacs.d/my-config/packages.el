(load "packages/evil.el")

;; Other packages

(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1))
(use-package base16-theme
  :config
  (load-theme 'base16-tomorrow-night t)
  (defun my/reload-dark ()
    (load-theme 'base16-tomorrow-night t)
    (defun my/get-color (base)
      (plist-get base16-tomorrow-night-colors base))
    (modify-face 'trailing-whitespace (my/get-color :base00) (my/get-color :base08))
    (modify-face 'line-number-current-line (my/get-color :base05) (my/get-color :base00) nil t)
    (modify-face 'line-number (my/get-color :base04) (my/get-color :base00)))
  (my/reload-dark)
  (defun blind-me ()
    (interactive)
    (if (custom-theme-enabled-p 'base16-tomorrow-night)
        (progn
          (disable-theme 'base16-tomorrow-night)
          (load-theme 'base16-tomorrow t))
      (progn
        (disable-theme 'base16-tomorrow)
        (my/reload-dark)))))
(use-package chess
  :pin gnu)
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
(use-package counsel
  :demand t
  :config
  (counsel-mode 1))
(use-package direnv
  :config
  (direnv-mode 1))
(use-package dockerfile-mode
  :mode "Dockerfile\\'")
(use-package edit-indirect) ;; For editing blocks inside markdown!
(use-package edit-server ;; https://www.emacswiki.org/emacs/Edit_with_Emacs
  :config
  (setq edit-server-new-frame nil)
  (edit-server-start))
(use-package flycheck
  :hook (lsp-ui-mode . flycheck-mode))
(use-package gist
  :commands (gist-region-private)
  :init
  (defun gist (start end)
    (interactive "r")
    (gist-region-private start end)))
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
                             (insert-buffer old-buffer)
                             (when (eq (shell-command-on-region (buffer-end 0) (buffer-end 1)
                                                                "goimports" (current-buffer) t
                                                                "GoImports Errors" t)
                                       0)
                               (buffer-string)))))
          (when new-content
            (delete-region (buffer-end 0) (buffer-end 1))
            (insert new-content)
            (goto-char old-pos)))))))
(use-package htmlize) ;; For org mode
(use-package hydra)
(use-package ivy
  :config
  (ivy-mode 1))
(use-package imenu-list
  :bind ("C-c i" . imenu-list-smart-toggle))
(use-package json-mode
  :mode "\\.json\\'")
(use-package lsp-mode
  :bind ("C-c e" . lsp-extend-selection)
  :commands lsp
  :hook (python-mode . lsp)
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-auto-guess-root t))
(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-max-width 50)
  (lsp-ui-doc-max-height 20))
(use-package markdown-mode
  :hook (markdown-mode . flycheck-mode)
  :mode "\\.md\\'"
  :custom
  (markdown-header-scaling t))
(use-package nasm-mode
  :hook (asm-mode . nasm-mode))
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
                                           (load "man")
                                           (let ((original-notify Man-notify-method))
                                             (setq Man-notify-method 'pushy)
                                             (man "configuration.nix")
                                             (setq Man-notify-method original-notify)))))
(use-package org
  :mode ("\\.org\\'". org-mode)
  :commands (org-mode)
  :custom ((org-startup-indented t)
           (org-startup-folded nil)))
(use-package powerline
  :config
  (powerline-center-evil-theme))
(use-package projectile
  :after projectile-ripgrep
  :demand t
  :custom ((projectile-completion-system 'ivy)
           (projectile-project-search-path '("~/" "~/Coding/Rust/" "~/Coding/Rust/external/")))
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
(use-package projectile-ripgrep)
(use-package ranger
  :bind ("C-c r" . ranger)
  :custom ((ranger-override-dired 'ranger)
           (ranger-override-dired-mode t)))
(use-package rustic
  :hook (rustic-mode . lsp)
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  (sp-local-pair 'rustic-mode "<" ">"))
(use-package rust-playground
  :after rustic-mode
  :commands (rust-playground rust-playground-mode))
(use-package slime
  :after slime-company
  :mode "\\.lisp\\'"
  :commands slime
  :config
  (setq inferior-lisp-program "sbcl --noinform")
  (slime-setup '(slime-fancy slime-company)))
(use-package slime-company)
(use-package smartparens
  :demand t
  :commands sp-local-pairs
  :bind (("C-M-l" . sp-forward-slurp-sexp)
         ("C-M-h" . sp-forward-barf-sexp))
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)

  ;; Allow using smartparens from minibuffer
  (setq sp-ignore-modes-list (remove 'minibuffer-inactive-mode sp-ignore-modes-list))

  ;; Configure evil to use smartparens for %
  (evil-define-motion my/matching-paren (num)
    :type inclusive
    (let* ((expr (sp-get-paired-expression))
           (begin (plist-get expr :beg))
           (next (plist-get expr :end))
           (end (if next (- next 1) nil)))
      (if (eq (point) end)
          (goto-char begin)
        (when end (goto-char end)))))
  (evil-global-set-key 'motion (kbd "%") 'my/matching-paren)

  ;; Create double newline on enter
  (defun my/newline-indent (&rest _ignored)
    "Call when newline is pressed - this will only add one newline"
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))
  (sp-local-pair 'prog-mode "{" nil :post-handlers '((my/newline-indent "RET")))
  (sp-local-pair 'prog-mode "(" nil :post-handlers '((my/newline-indent "RET")))
  (sp-local-pair 'prog-mode "[" nil :post-handlers '((my/newline-indent "RET"))))
(use-package string-inflection
  :after transient
  :config
  (defhydra my/string-inflection-keys (global-map "C-c")
    "
Toggle string casing
--------------------
[_s_]: snake%(quote _)case
[_S_]: SCREAMING%(quote _)SNAKE%(quote _)CASE
[_k_]: kebab-case
[_c_]: camelCase
[_C_]: PascalCase
"
    ("_" string-inflection-cycle "Cycle common")
    ("-" string-inflection-all-cycle "Cycle all" :bind nil)
    ("s" string-inflection-underscore :bind nil)
    ("S" string-inflection-upcase :bind nil)
    ("k" string-inflection-kebab-case :bind nil)
    ("c" string-inflection-lower-camelcase :bind nil)
    ("C" string-inflection-camelcase :bind nil)))
(use-package sublimity
  :config
  (require 'sublimity-scroll)
  (sublimity-mode 1))
(use-package auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :pin gnu)
(use-package yaml-mode
  :mode "\\.yml\\'")
(use-package yasnippet
  :demand t
  :config
  (setq yas-snippet-dirs (list (my/relative "snippets")))
  (yas-global-mode 1))
