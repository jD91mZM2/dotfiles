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
(setq bookmark-save-flag 1)

;; Transparency!
(add-to-list 'default-frame-alist '(alpha . 90)) ; default frame settings
(set-frame-parameter (selected-frame) 'alpha 90) ; for current session

;; Load file templates
(load "templates")

;; # Editing options
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)
(setq-default tab-width 4)
(setq-default truncate-lines t)
(setq-default vc-follow-symlinks t)
(modify-syntax-entry ?_ "w") ; Make _ a word character, like it is in vim

(electric-pair-mode 1)
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)
(show-paren-mode 1)

;; # Keybindings
;; See https://www.emacswiki.org/emacs/DvorakKeyboard. To avoid all
;; conflicts, this translates the keys earlier in the stack. Using
;; keyboard-translate, however, is not possible because it does not
;; work well with emacsclient.
(define-key key-translation-map (kbd "C-t") (kbd "C-x"))

(defun my/set-compile (cmd)
  "Function used in packages.el to set the compilation command to a custom one"
  (set (make-local-variable 'compile-command) cmd))

(define-key global-map (kbd "C-c c") 'recompile)
(define-key emacs-lisp-mode-map (kbd "C-c c") 'eval-buffer)

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
(load "packages")