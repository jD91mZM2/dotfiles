(eval-when-compile (require 'use-package))
(require 'evil)
(require 'man)
(require 'my-styling)

;; General language support

(use-package flycheck
  :hook (lsp-ui-mode . flycheck-mode)
  :commands (flycheck-add-mode))
(use-package lsp-mode
  :bind ("C-c e" . lsp-extend-selection)
  :commands (lsp lsp-register-client)
  :hook (python-mode . lsp)
  :custom
  (lsp-enable-indentation nil)
  (lsp-prefer-flymake nil)
  (lsp-auto-guess-root t)
  (lsp-restart 'ignore))
(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-max-width 50)
  (lsp-ui-doc-max-height 20)
  (lsp-ui-doc-position 'top))

;; Optional dependencies

(use-package htmlize) ;; org-mode: Export to HTML
(use-package edit-indirect) ;; markdown-mode: Edit code blocks

;; More specific modes

;; Emacs Lisp
(my/util/font-lock-extend 'emacs-lisp-mode '(("[[:space:](]+\\(t\\)[[:space:])]+" 1 font-lock-builtin-face)
                                             ("[[:space:](]+\\(nil\\)[[:space:])]+" 1 font-lock-builtin-face)
                                             ("[[:space:](]+\\(-?[0-9]+\\)[[:space:])]+" 1 font-lock-builtin-face)
                                             ("[[:space:](]+\\('[a-zA-Z-]+\\)[[:space:])]+" 1 font-lock-constant-face)
                                             ("[()]" . font-lock-comment-face)))

;; LaTeX
(use-package auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :custom
  (TeX-command-list '(("LatexMk" "latexmk -pdf %t; pgrep zathura || zathura --fork %s.pdf" TeX-run-command nil t)))
  (TeX-command-force "LatexMk"))

;; Dot
(use-package graphviz-dot-mode
  :mode "\\.dot"
  :config
  (require 'company-graphviz-dot))

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

;; Haskell
(use-package haskell-mode
  :hook (haskell-mode . flycheck-mode)
  :mode "\\.hs\\'")

;; Lua
(use-package lua-mode
  :mode "\\.lua\\'")

;; Markdown
(use-package markdown-mode
  :hook (markdown-mode . flycheck-mode)
  :mode "\\.md\\'"
  :custom
  (markdown-header-scaling t)
  (markdown-fontify-code-blocks-natively t))

;; Minecraft Functions
(use-package mcf-mode
  :mode "\\.mcfunction\\'")

;; Nasm
(use-package nasm-mode
  :hook (asm-mode . nasm-mode))

;; Nix
(use-package nix-mode
  :after (lsp)
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
  :commands (org-mode org-display-inline-images org-remove-inline-images)
  :custom
  (org-startup-indented t)
  (org-startup-folded nil)
  (org-list-allow-alphabetical t))
(use-package org-present
  :after org
  :commands (org-present org-present-big org-present-hide-cursor
                         org-present-show-cursor org-present-read-only
                         org-present-read-write org-present-small)
  :config
  (add-hook 'org-present-mode-hook
            (defun my/org-present-start ()
              (org-present-big)
              (org-display-inline-images)
              (org-present-hide-cursor)
              (org-present-read-only)
              (display-line-numbers-mode -1)
              ;; doing hl-line-mode locally isn't enough
              (global-hl-line-mode -1)
              (setq-local mode-line-format nil)
              (evil-emacs-state nil)))
  (add-hook 'org-present-mode-quit-hook
            (defun my/org-present-end ()
              (org-present-small)
              (org-remove-inline-images)
              (org-present-show-cursor)
              (org-present-read-write)
              (display-line-numbers-mode 1)
              (global-hl-line-mode 1)
              (setq-local mode-line-format (my/modeline))
              (evil-exit-emacs-state nil))))
(use-package ob-rust
  :after org)

;; Rust
(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :hook (rustic-mode . lsp)
  :custom
  (rustic-lsp-setup-p nil)
  (rustic-match-angle-brackets nil)
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
  :commands (slime slime-setup)
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
         "\\.less\\'"
         "\\.mcmeta\\'")
  :custom
  (web-mode-content-types-alist '(("json" . "\\.mcmeta\\'")))
  :config
  (flycheck-add-mode 'json-python-json 'web-mode)
  (add-hook 'web-mode-hook (defun my/web-mode-hook ()
                             (when (equal web-mode-content-type "json")
                               (flycheck-mode 1)
                               (setq-local web-mode-code-indent-offset 2)
                               (setq-local tab-width 2)))))

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
  '("module" "func" "param" "result" "export" "type" "return" "call" "br_if" "br")
  '(("[()]" . font-lock-comment-face)
    ("\\<\\(i32\\|i64\\|f32\\|f64\\)\\>" . font-lock-type-face)
    ("\\$\\([a-zA-Z_][a-zA-Z_0-9]*\\)" . font-lock-constant-face)
    ("(\\(local\\)" 1 font-lock-variable-name-face)
    ("\\<\\(?:if\\|loop\\|block\\|end\\)\\>" . font-lock-builtin-face))
  '("\\.wat\\'")
  '((lambda ()
      (setq-local tab-width 2)))
  "WebAssembly Text format")

(provide 'my-languages)
