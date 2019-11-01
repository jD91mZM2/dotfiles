(load "packages/evil.el")
(load "packages/completion.el")
(load "packages/languages.el")
(load "packages/navigation.el")
(load "packages/editing.el")

;; Styling

(use-package base16-theme
  :demand t
  :config
  (load-theme 'base16-tomorrow-night t)
  (defun my/reload-dark ()
    (load-theme 'base16-tomorrow-night t)
    (defun my/get-color (base)
      (plist-get base16-tomorrow-night-colors base))
    (modify-face 'trailing-whitespace (my/get-color :base00) (my/get-color :base08))
    (modify-face 'line-number-current-line (my/get-color :base05) (my/get-color :base00) nil t)
    (modify-face 'line-number (my/get-color :base04) (my/get-color :base00)))
  (my/reload-dark))

(use-package powerline
  :demand t
  :config
  (powerline-center-evil-theme))

;; Packages needed elsewhere

(use-package hydra
  :demand t)

(require 'man)

;; Packages that I for other reasons want always available

(use-package counsel
  :demand t
  :config
  (counsel-mode 1))
(use-package direnv
  :demand t
  :config
  (direnv-mode 1))
(use-package edit-server ;; https://www.emacswiki.org/emacs/Edit_with_Emacs
  :demand t
  :config
  (setq edit-server-new-frame nil)
  (edit-server-start))
(use-package ivy
  :demand t
  :config
  (ivy-mode 1))

;; Other packages

(use-package chess
  :commands chess
  :pin gnu)
(use-package gist
  :commands gist-region-private
  :init
  (defun gist (start end)
    (interactive "r")
    (gist-region-private start end)))
(use-package sublimity
  :config
  (require 'sublimity-scroll)
  (sublimity-mode 1))
(use-package uuid
  :commands uuid-string)
