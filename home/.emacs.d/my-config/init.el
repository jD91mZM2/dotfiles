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

(define-key emacs-lisp-mode-map (kbd "C-c c") 'eval-buffer)
(global-set-key (kbd "C-c b") 'bookmark-bmenu-list)
(global-set-key (kbd "C-c c") 'recompile)

;; Shell madness
(global-set-key (kbd "C-c s") (lambda ()
                                (interactive)
                                (term (or (getenv "SHELL" ) "/bin/sh"))))
(add-hook 'term-mode-hook
          (defun my/term-hook ()
            (setq show-trailing-whitespace nil)))
(add-hook 'eshell-mode-hook
          (defun my/eshell-hook ()
            (evil-define-key 'insert eshell-mode-map (kbd "C-d")
              (lambda ()
                (interactive)
                (unless (eshell-send-eof-to-process)
                  (kill-buffer))))))

;; # Set up emacsclient the way I want it
(setq confirm-kill-emacs 'y-or-n-p)
(server-start)
(add-hook 'server-switch-hook
          (defun my/server-switch-hook ()
            ; Ask xdotool to switch to the emacs window
            (call-process "xdotool" nil nil nil "windowactivate" (frame-parameter nil 'outer-window-id))))

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
