;; General language support

(use-package flycheck
  :hook (lsp-ui-mode . flycheck-mode)
  :commands (flycheck-add-mode))
(use-package lsp-mode
  :bind ("C-c e" . lsp-extend-selection)
  :commands (lsp lsp-register-client)
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
(my/util/font-lock-extend 'emacs-lisp-mode '(("[[:space:](]+\\(t\\)[[:space:])]+" 1 font-lock-builtin-face)
                                             ("[[:space:](]+\\(nil\\)[[:space:])]+" 1 font-lock-builtin-face)
                                             ("[[:space:](]+\\(-?[0-9]+\\)[[:space:])]+" 1 font-lock-builtin-face)
                                             ("[[:space:](]+\\('[a-zA-Z-]+\\)[[:space:])]+" 1 font-lock-constant-face)
                                             ("[()]" . font-lock-comment-face)))

;; LaTeX
(use-package auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :pin gnu)

;; Dhall
(use-package dhall-mode
  :mode "\\.dhall\\'")

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

;; Markdown
(use-package markdown-mode
  :hook (markdown-mode . flycheck-mode)
  :mode "\\.md\\'"
  :custom
  (markdown-header-scaling t))

;; Minecraft Functions
(use-package mcf-mode
  :ensure quelpa
  :quelpa (mcf
           :fetcher github
           :repo "rasensuihei/mcf"
           :commit "7fe4c74a47b1820806bf92d818039dafb2df114b")
  :mode "\\.mcfunction\\'")

;; Nasm
(use-package nasm-mode
  :hook (asm-mode . nasm-mode))

;; Nix
(use-package nix-mode
  :hook (nix-mode . lsp)
  :mode "\\.nix\\'"
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("bash" "-c" "env RUST_LOG=trace rnix-lsp 2> /tmp/nix-lsp.log"))
                    :major-modes '(nix-mode)
                    :server-id 'nix))
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (setq nix-mode-use-smie t)
  (define-key nix-mode-map (kbd "C-M-x") (lambda (beg end)
                                           (interactive "r")
                                           (shell-command-on-region beg end "nix-instantiate --eval -")))
  (define-key nix-mode-map (kbd "C-c m") (lambda ()
                                           (interactive)
                                           (let ((original-notify Man-notify-method))
                                             (setq Man-notify-method 'pushy)
                                             (man "configuration.nix")
                                             (setq Man-notify-method original-notify))))
  (my/util/font-lock-extend 'nix-mode '(("\\<builtins.[a-zA-Z]+\\>" . font-lock-builtin-face))))

;; Org
(use-package org
  :mode ("\\.org\\'". org-mode)
  :commands org-mode
  :custom ((org-startup-indented t)
           (org-startup-folded nil)
           (org-list-allow-alphabetical t)))
(use-package ob-ipython :after org)
(use-package ob-rust    :after org)

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

;; JSON, JavaScript, HTML, CSS, etc. This is an AWESOME mode which I highly recommend
(use-package web-mode
  :mode ("\\.html?\\'"
         "\\.jsx?\\'"
         "\\.tsx?\\'"
         "\\.json\\'"
         "\\.s?css\\'"
         "\\.less\\'")
  :hook (web-mode . lsp)
  :config
  (flycheck-add-mode 'json-python-json 'web-mode))

;; Python
(add-hook 'python-mode-hook (defun my/python-hook ()
                              (setq-local fill-column 80)))

;; YAML
(use-package yaml-mode
  :mode "\\.yml\\'")

;; Other languages
(require 'generic-x)

(define-generic-mode lark-mode
  '("//")
  '("%import" "%ignore")
  '(("[^/]\\(/[^/]\\(\\\\/\\|[^/]\\)*/\\)" (1 font-lock-constant-face))
    ("\\([a-zA-Z0-9_]+\\):" (1 font-lock-builtin-face))
    ("~[0-9]+\\|[?+*]" . font-lock-builtin-face)
    ("|" . font-lock-type-face))
  '("\\.lark\\'")
  nil
  "A mode for files read by the LARK python parser library")

(define-generic-mode wat-mode
  '(";;")
  '("module" "func" "param" "result" "export")
  '(("[()]" . font-lock-comment-face)
    ("\\<\\(i32\\|i64\\|f32\\|f64\\)\\>" . font-lock-type-face)
    ("\\$\\([a-zA-Z_]+\\)" . font-lock-constant-face))
  '("\\.wat\\'")
  '((lambda ()
      (setq-local tab-width 2)))
  "WebAssembly Text format")
