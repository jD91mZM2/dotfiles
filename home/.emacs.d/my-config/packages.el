(use-package base16-theme
  :config
  (load-theme 'base16-tomorrow-night t)
  (defun my/get-color (base)
    (plist-get base16-tomorrow-night-colors base))
  (modify-face 'trailing-whitespace "#000000" (my/get-color :base08))
  (modify-face 'line-number-current-line (my/get-color :base00) (my/get-color :base03)))
(use-package company
  :config
  (global-company-mode 1)
  (setq company-idle-delay 0))
(use-package company-auctex)
(use-package company-lsp)
(use-package evil
  :init
  (setq-default evil-want-keybinding nil)
  (setq-default evil-want-C-u-scroll t)
  (setq-default evil-search-module 'evil-search)
  (setq-default evil-ex-search-persistent-highlight nil)
  :config
  (evil-mode 1)
  (evil-define-key 'normal global-map "gt" 'switch-to-buffer)
  (advice-add 'evil-quit :around (lambda (&rest _orig) (kill-buffer nil)))

  ;; Disable search after duration
  (defvar my/stop-hl-timer-last nil)
  (defun my/stop-hl-timer (_)
    (when my/stop-hl-timer-last
      (cancel-timer my/stop-hl-timer-last))
    (setq my/stop-hl-timer-last
          (run-at-time 1 nil (lambda () (evil-ex-nohighlight)))))
  (advice-add 'evil-ex-search-activate-highlight :after 'my/stop-hl-timer))
(use-package evil-collection
  :config
  (evil-collection-init))
(use-package evil-magit
  :after evil
  :config
  (evil-define-key 'normal global-map (kbd "C-s") 'magit-status))
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))
(use-package flycheck
  :hook (lsp-ui-mode . flycheck-mode))
(use-package ivy
  :config
  (ivy-mode 1))
(use-package json-mode
  :mode "\\.json\\'")
(use-package lsp-mode
  :hook ((rust-mode nix-mode) . lsp)
  :config
  (setq lsp-prefer-flymake nil))
(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-max-width 50)
  (lsp-ui-doc-max-height 20))
(use-package neotree
  :after evil
  :config
  (setq-default neotree-smart-open t)
  (defun neotree-no-focus ()
    (neotree)
    (neotree-find)
    (other-window 1))
  (if (daemonp)
      (add-hook 'server-switch-hook 'neotree-no-focus) ; emacs
    (add-hook 'emacs-startup-hook 'neotree-no-focus))) ; emacsclient
(use-package nix-mode
  :after lsp-mode
  :mode "\\.nix\\'"
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "nix-lsp")
                    :major-modes '(nix-mode)
                    :server-id 'nix)))
(use-package powerline
  :config
  (powerline-center-evil-theme))
(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (add-hook 'rust-mode-hook (lambda () (my/set-compile "cargo run"))))
(use-package tex
  :ensure auctex ; I have no idea why using use-package auctex does not work
  :pin gnu)
