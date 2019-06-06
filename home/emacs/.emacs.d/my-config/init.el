(add-to-list 'load-path (locate-user-emacs-file "my-config") t)

;;  _____                                        _   _
;; | ____|_ __ ___   __ _  ___ ___    ___  _ __ | |_(_) ___  _ __  ___
;; |  _| | '_ ` _ \ / _` |/ __/ __|  / _ \| '_ \| __| |/ _ \| '_ \/ __|
;; | |___| | | | | | (_| | (__\__ \ | (_) | |_) | |_| | (_) | | | \__ \
;; |_____|_| |_| |_|\__,_|\___|___/  \___/| .__/ \__|_|\___/|_| |_|___/
;;                                        |_|

;; Prevent emacs from writing stuff to this file. All customization
;; options should be set from here, and all packages should be set
;; using use-package.
(setq custom-file "/dev/null")

(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file "backups/"))))
(setq auto-save-file-name-transforms `((".*" ,(locate-user-emacs-file "backups/") t)))

(setq inhibit-startup-screen t)
(setq bookmark-save-flag 1)
(setq mouse-wheel-progressive-speed nil)

;; Transparency!
(add-to-list 'default-frame-alist '(alpha . 90)) ; default frame settings
(set-frame-parameter (selected-frame) 'alpha 90) ; for current session

;; Load file templates
(load "templates")

;; # Editing options
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(setq-default require-final-newline t)
(setq-default show-trailing-whitespace t)
(setq-default tab-width 4)
(setq-default truncate-lines t)
(setq-default vc-follow-symlinks t)
(setq-default display-line-numbers-type 'relative)

(add-hook 'after-change-major-mode-hook
          (defun my/major-hook ()
            (modify-syntax-entry ?_ "w"))) ; Make _ a word character in all syntaxes, like it is in vim

(electric-pair-mode 1)
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(tool-bar-mode -1)

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
(global-set-key (kbd "C-c s") (lambda ()
                                (interactive)
                                (term (or (getenv "SHELL" ) "/bin/sh"))))
(global-set-key (kbd "C-c a") (lambda (start end)
                                (interactive "r")
                                (shell-command-on-region start end "figlet" (current-buffer) t)
                                (comment-region (mark) (point))))

;; Shell madness
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

;;  ____            _
;; |  _ \ __ _  ___| | ____ _  __ _  ___  ___
;; | |_) / _` |/ __| |/ / _` |/ _` |/ _ \/ __|
;; |  __/ (_| | (__|   < (_| | (_| |  __/\__ \
;; |_|   \__,_|\___|_|\_\__,_|\__, |\___||___/
;;                            |___/

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
