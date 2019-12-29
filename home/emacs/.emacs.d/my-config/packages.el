(load "packages/evil.el")
(load "packages/completion.el")
(load "packages/languages.el")
(load "packages/navigation.el")
(load "packages/editing.el")

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
