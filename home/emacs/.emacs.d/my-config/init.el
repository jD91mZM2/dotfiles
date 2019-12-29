(add-to-list 'load-path (locate-user-emacs-file "my-config") t)

(load "utils")
(load "devices")

;;  ____            _
;; |  _ \ __ _  ___| | ____ _  __ _  ___  ___
;; | |_) / _` |/ __| |/ / _` |/ _` |/ _ \/ __|
;; |  __/ (_| | (__|   < (_| | (_| |  __/\__ \
;; |_|   \__,_|\___|_|\_\__,_|\__, |\___||___/
;;                            |___/

(eval-when-compile
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (setq package-enable-at-startup nil)
  (package-initialize)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (require 'use-package))
(setq use-package-always-ensure t)
(setq use-package-always-pin "melpa")

(load "utils")
(load "packages")

;;  _____                                        _   _
;; | ____|_ __ ___   __ _  ___ ___    ___  _ __ | |_(_) ___  _ __  ___
;; |  _| | '_ ` _ \ / _` |/ __/ __|  / _ \| '_ \| __| |/ _ \| '_ \/ __|
;; | |___| | | | | | (_| | (__\__ \ | (_) | |_) | |_| | (_) | | | \__ \
;; |_____|_| |_| |_|\__,_|\___|___/  \___/| .__/ \__|_|\___/|_| |_|___/
;;                                        |_|

;; Prevent emacs from writing stuff to this file. All customization
;; options should be set from here, and all packages should be set
;; using use-package.
(setq custom-file null-device)

(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file "backups/"))))
(setq auto-save-file-name-transforms `((".*" ,(locate-user-emacs-file "backups/") t)))

;; General options
(setq-default bookmark-save-flag t)
(setq-default delete-by-moving-to-trash t)
(setq-default inhibit-startup-screen t)
(setq-default mouse-wheel-progressive-speed nil)

;; Load file templates
(load "templates")

;; Editing options
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(setq-default require-final-newline t)
(setq-default tab-width 4)
(setq-default vc-follow-symlinks t)

;;  ____  _         _ _
;; / ___|| |_ _   _| (_)_ __   __ _
;; \___ \| __| | | | | | '_ \ / _` |
;;  ___) | |_| |_| | | | | | | (_| |
;; |____/ \__|\__, |_|_|_| |_|\__, |
;;            |___/           |___/

(setq-default display-line-numbers-type 'relative)
(setq-default glasses-uncapitalize-p t)
(setq-default show-trailing-whitespace t)

(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Fonts
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

;; Transparency!
(add-to-list 'default-frame-alist (cons 'alpha my/option/transparency)) ; default frame settings
(set-frame-parameter (selected-frame) 'alpha my/option/transparency)    ; for current session

(desktop-save-mode 1)
(xterm-mouse-mode 1)

(use-package dracula-theme
  :demand t
  :config
  (load-theme 'dracula t))

(use-package powerline
  :demand t
  :config
  (powerline-center-evil-theme))

;;  _  __          _     _           _
;; | |/ /___ _   _| |__ (_)_ __   __| |___
;; | ' // _ \ | | | '_ \| | '_ \ / _` / __|
;; | . \  __/ |_| | |_) | | | | | (_| \__ \
;; |_|\_\___|\__, |_.__/|_|_| |_|\__,_|___/
;;           |___/

;; Swap C-t and C-x to make it easier on dvorak. This is applied
;; earliest possible and will work basically everywhere. It doesn't
;; work in all emacsclient-sessions, unlike `key-translation-map', but
;; it does solve some rare issues with the latter alternative that
;; makes it worth battling the quirks.
(keyboard-translate ?\C-t ?\C-x)
(keyboard-translate ?\C-x ?\C-t)

(define-key emacs-lisp-mode-map (kbd "C-c c") 'eval-buffer)
(global-set-key (kbd "C-c b") 'bookmark-bmenu-list)
(global-set-key (kbd "C-c c") 'recompile)
(global-set-key (kbd "C-c s") 'eshell)

;;  __  __           _ _  __         _          _                 _
;; |  \/  | ___   __| (_)/ _|_   _  | |__   ___| |__   __ ___   _(_) ___  _ __
;; | |\/| |/ _ \ / _` | | |_| | | | | '_ \ / _ \ '_ \ / _` \ \ / / |/ _ \| '__|
;; | |  | | (_) | (_| | |  _| |_| | | |_) |  __/ | | | (_| |\ V /| | (_) | |
;; |_|  |_|\___/ \__,_|_|_|  \__, | |_.__/ \___|_| |_|\__,_| \_/ |_|\___/|_|
;;                           |___/

(add-hook 'after-change-major-mode-hook
          (defun my/major-hook ()
            (modify-syntax-entry ?_ "w"))) ; Make _ a word character in all syntaxes, like it is in vim

(add-hook 'text-mode-hook (defun my/text-hook ()
                            (auto-fill-mode 1)))

(add-hook 'term-mode-hook
          (defun my/term-hook ()
            (setq show-trailing-whitespace nil)))

(add-hook 'desktop-after-read-hook
          (defun my/desktop-read-hook ()
            (clean-buffer-list)))

;;   ____                                          _
;;  / ___|___  _ __ ___  _ __ ___   __ _ _ __   __| |___
;; | |   / _ \| '_ ` _ \| '_ ` _ \ / _` | '_ \ / _` / __|
;; | |__| (_) | | | | | | | | | | | (_| | | | | (_| \__ \
;;  \____\___/|_| |_| |_|_| |_| |_|\__,_|_| |_|\__,_|___/

;; Minor modes

(define-minor-mode keep-centered-mode
  "Keep recentering the screen all the time why not"
  :init-value nil
  (add-hook 'post-command-hook
            (defun my/keep-centered-hook ()
              (when keep-centered-mode
                (recenter nil)))))

;; Eshell commands

(defun eshell/e (file)
  "Eshell alias to open a file inside emacs"
  (interactive)
  (find-file file))

;; Other commands

(defun touch ()
  "Touch the current file"
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

(defun uuid ()
  "Insert a new random UUID"
  (interactive)
  (insert (uuid-string)))

;;  _____                               _ _            _
;; | ____|_ __ ___   __ _  ___ ___  ___| (_) ___ _ __ | |_
;; |  _| | '_ ` _ \ / _` |/ __/ __|/ __| | |/ _ \ '_ \| __|
;; | |___| | | | | | (_| | (__\__ \ (__| | |  __/ | | | |_
;; |_____|_| |_| |_|\__,_|\___|___/\___|_|_|\___|_| |_|\__|

;; Set up emacsclient the way I want it
(setq confirm-kill-emacs 'y-or-n-p)
(server-start)
(add-hook 'server-switch-hook
          (defun my/server-switch-hook ()
            ;; Ask xdotool to switch to the emacs window
            (let ((window-id (frame-parameter nil 'outer-window-id)))
              (when window-id
                (condition-case nil
                    (call-process "xdotool" nil 0 nil "windowactivate" window-id)
                  ('file-missing (message "Failed to execute xdotool, ignoring")))))))
