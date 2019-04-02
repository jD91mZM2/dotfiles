(use-package airline-themes
  :after base16-theme
  :config
  (load-theme 'airline-base16-gui-dark t)
  (airline-themes-set-modeline))
(use-package base16-theme
  :config
  (load-theme 'base16-tomorrow-night t)
  (modify-face 'trailing-whitespace "#000000" (plist-get base16-tomorrow-night-colors :base08)))
(use-package company
  :config
  (company-mode 1))
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
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))
(use-package ivy
  :config
  (ivy-mode 1))
(use-package json-mode)
(use-package lsp-mode
  :after rust-mode
  :config
  (add-hook 'rust-mode-hook 'lsp))
(use-package lsp-ui
  :after lsp-ui
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))
(use-package neotree
  :after evil
  :config
  (setq-default neotree-smart-open t)
  (add-hook
   'emacs-startup-hook
   (lambda ()
     (neotree)
     (select-window (window-right (neo-global--get-window))))))
(use-package nix-mode)
(use-package nixos-options)
(use-package ranger)
(use-package rust-mode)
(use-package swiper)
