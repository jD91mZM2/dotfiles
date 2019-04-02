;; ------------------------------
;;   Configure general options
;; ------------------------------

(add-to-list 'load-path (locate-user-emacs-file "my-config") t)

;; # Emacs options

;; Prevent emacs from writing stuff to this file. All customization
;; options should be set from here, and all packages should be set
;; using use-package.
(setq custom-file "/dev/null")

(setq make-backup-files nil)
(setq inhibit-startup-screen t)

;; Transparency!
(set-frame-parameter (selected-frame) 'alpha 90)

;; Load file templates
(load "templates.el")

;; # Editing options
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)
(setq-default tab-width 4)
(electric-pair-mode 1)
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)

;; ------------------------------
;;       Configure packages
;; ------------------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-always-pin "melpa")
(load "packages.el")
