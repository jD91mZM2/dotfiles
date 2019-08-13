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
(use-package cargo
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode))
(use-package company
  :after evil
  :config
  (global-company-mode 1)
  (setq company-idle-delay 0)
  (evil-define-key 'insert 'company-mode-hook (kbd "C-n") 'company-select-next-if-tooltip-visible-or-complete-selection)
  (evil-define-key 'insert 'company-mode-hook (kbd "C-p") 'company-select-previous))
(use-package company-auctex)
(use-package company-lsp)
(use-package counsel
  :demand t
  :bind (("C-c j" . counsel-imenu)
         ("C-c i" . counsel-info-lookup-symbol))
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
(use-package evil
  :init
  (setq-default evil-want-keybinding nil)
  (setq-default evil-want-C-u-scroll t)
  (setq-default evil-search-module 'evil-search)
  (setq-default evil-ex-search-persistent-highlight nil)
  :config
  (evil-mode 1)
  (global-undo-tree-mode -1)
  (evil-global-set-key 'normal (kbd "gt") 'switch-to-buffer)
  (evil-global-set-key 'normal (kbd "gcc") 'comment-or-uncomment-region)
  (evil-global-set-key 'normal (kbd "gcw") 'delete-trailing-whitespace)
  (evil-global-set-key 'normal (kbd "D") (lambda ()
                                           (interactive)
                                           (beginning-of-line)
                                           (kill-line)))
  (evil-global-set-key 'normal (kbd "gca")
                       (lambda (start end)
                         (interactive "r")
                         (shell-command-on-region start end "figlet" (current-buffer) t)
                         (comment-region (mark) (point))))
  (evil-global-set-key 'normal (kbd "gyf") (lambda ()
                                             (interactive)
                                             (kill-new (buffer-file-name))
                                             (message "%s" (buffer-file-name))))
  (evil-global-set-key 'insert (kbd "C-c d")
                       (lambda (prefix)
                         (interactive "P")
                         (insert (format-time-string
                                  (if prefix
                                      (let* ((seconds (car (current-time-zone)))
                                             (minutes (/ seconds 60))
                                             (hours   (/ minutes 60)))
                                        (concat "%FT%T" (format "%+.2d:%.2d" hours (% minutes 60))))
                                    "%F")))))

  (evil-global-set-key 'insert (kbd "C-c n")
                       (lambda (num)
                         (interactive "nInput start number: ")
                         (kmacro-set-counter num)))

  ;; Disable search highlights after short duration
  (defvar my/stop-hl-timer-last nil)
  (defun my/stop-hl-timer (_)
    (when my/stop-hl-timer-last
      (cancel-timer my/stop-hl-timer-last))
    (setq my/stop-hl-timer-last
          (run-at-time 1 nil (lambda () (evil-ex-nohighlight)))))
  (advice-add 'evil-ex-search-activate-highlight :after 'my/stop-hl-timer)

  (evil-set-initial-state 'ses-mode 'emacs))
(use-package evil-args
  :after evil
  :config
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))
(use-package evil-easymotion
  :after evil
  :config
  (evilem-default-keybindings "SPC"))
(use-package evil-magit
  :after evil
  :bind ("C-c g" . magit-status)
  :demand t)
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))
(use-package flycheck
  :hook (lsp-ui-mode . flycheck-mode))
(use-package gist
  :bind ("C-c y" . gist-region-private))
(use-package go-mode
  :config
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
(use-package ivy
  :config
  (ivy-mode 1))
(use-package json-mode
  :mode "\\.json\\'")
(use-package lsp-mode
  :hook ((go-mode nix-mode python-mode rust-mode) . lsp)
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-auto-guess-root t))
(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-max-width 50)
  (lsp-ui-doc-max-height 20))
(use-package neotree
  :bind ("C-c t" . neotree-toggle)
  :custom ((neo-smart-open t)
           (neo-show-hidden-files t))
  :config
  (add-hook 'neotree-mode-hook (defun my/neotree-hook ()
                            (display-line-numbers-mode -1)))
  (advice-add 'neo-open-file :after
              (defun my/neotree-open (_orig &rest _args)
                  (neotree-hide))))
(use-package nix-mode
  :after lsp-mode
  :mode "\\.nix\\'"
  :config
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("nix-lsp"))
                    :major-modes '(nix-mode)
                    :server-id 'nix))
  (setq nix-mode-use-smie t)
  (define-key nix-mode-map (kbd "C-M-x") (lambda (start end)
                                             (interactive "r")
                                             (shell-command-on-region start end "nix-instantiate --eval -")))
  (define-key nix-mode-map (kbd "C-c m") (lambda ()
                                           (interactive)
                                           (load "man")
                                           (let ((original-notify Man-notify-method))
                                             (setq Man-notify-method 'pushy)
                                             (man "configuration.nix")
                                             (setq Man-notify-method original-notify)))))
(use-package org)
(use-package powerline
  :config
  (powerline-center-evil-theme))
(use-package projectile
  :custom (projectile-completion-system 'ivy)
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
(use-package ranger
  :bind ("C-c r" . ranger)
  :custom ((ranger-override-dired 'ranger)
           (ranger-override-dired-mode t)))
(use-package rust-mode
  :mode "\\.rs\\'")
(use-package rust-playground
  :after rust-mode
  :commands (rust-playground rust-playground-mode))
(use-package smartparens
  :after evil
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
(use-package sublimity
  :config
  (require 'sublimity-scroll)
  (sublimity-mode 1))
(use-package tex
  :ensure auctex ; I have no idea why using use-package auctex does not work
  :pin gnu)
(use-package yaml-mode
  :mode "\\.yml\\'")
