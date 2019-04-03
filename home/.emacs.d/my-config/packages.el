(use-package base16-theme
  :config
  (load-theme 'base16-tomorrow-night t)
  (modify-face 'trailing-whitespace "#000000" (plist-get base16-tomorrow-night-colors :base08)))
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
  :config
  (evil-mode 1)
  (evil-define-key 'normal global-map "gt" 'switch-to-buffer))
(use-package evil-collection
  :config
  (evil-collection-init))
(use-package evil-magit
  :bind ("C-s" . 'magit-status))
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))
(use-package ivy
  :config
  (ivy-mode 1))
(use-package json-mode)
(use-package lsp-mode
  :after rust-mode
  :hook (rust-mode . lsp))
(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode))
(use-package neotree
  :after evil
  :config
  (setq-default neotree-smart-open t)
  (defun neotree-no-focus ()
    (neotree)
    (neotree-find)
    (other-window 1))
  (if (daemonp)
    (add-hook 'server-switch-hook 'neotree-no-focus)   ; emacs
    (add-hook 'emacs-startup-hook 'neotree-no-focus))) ; emacsclient
(use-package nix-mode)
(use-package powerline
  :config
  (powerline-center-evil-theme))
(use-package ranger)
(use-package rust-mode)
(use-package swiper)
(use-package tex
  :ensure auctex ; I have no idea why using use-package auctex does not work
  :pin gnu)
