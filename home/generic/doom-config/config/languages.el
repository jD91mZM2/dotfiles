;;; languages.el -*- lexical-binding: t; -*-

;; Assembler
(use-package! nasm-mode
  :mode "\\.asm\\'"
  :hook (nasm-mode . aggressive-indent-mode)
  :config
  (setq! nasm-after-mnemonic-whitespace :space))

;; Rust
(use-package! rustic
  :config
  ;; Angle-bracket matching breaks bit-shifting
  (setq! rustic-match-angle-brackets nil)
  ;; Use rust-analyzer
  (setq! rustic-lsp-server 'rust-analyzer))

;; Minecraft .mcfunction mode
(use-package! mcf-mode
  :mode "\\.mcfunction\\'")

;; LaTeX
(use-package auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :custom
  (TeX-command-list '(("LatexMk" "latexmk -pdf %t; pgrep zathura || zathura --fork %s.pdf" TeX-run-command nil t)))
  (TeX-command-force "LatexMk"))

;; Web stuff
(use-package! web-mode
  :mode "\\.\\(json\\|php\\|html\\|jsx?\\|tsx?\\)\\'")

;; YAML
(use-package! yaml-mode
  :mode "\\.yaml")

;; XML
(use-package! nxml-mode
  :config
  ;; Somewhere, somehow, this is changed to 4. I don't like it.
  (setq-hook! nxml-mode
    nxml-child-indent 2))

;; Custom major modes using the legendary define-generic-mode:

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
