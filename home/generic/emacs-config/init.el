;;  ____       _
;; |  _ \  ___| |__  _   _  __ _    ___  _ __     ___ _ __ _ __ ___  _ __
;; | | | |/ _ \ '_ \| | | |/ _` |  / _ \| '_ \   / _ \ '__| '__/ _ \| '__|
;; | |_| |  __/ |_) | |_| | (_| | | (_) | | | | |  __/ |  | | | (_) | |
;; |____/ \___|_.__/ \__,_|\__, |  \___/|_| |_|  \___|_|  |_|  \___/|_|
;;                         |___/

;; Searches can fail, calm down
(add-to-list 'debug-ignored-errors 'search-failed)

;; Don't annoy me when I'm typing
(add-to-list 'debug-ignored-errors "^use-package: Unrecognized")

(setq debug-on-error t)

;;  ____       _                              _ _
;; / ___|  ___| |_   _   _ _ __     _____   _(_) |
;; \___ \ / _ \ __| | | | | '_ \   / _ \ \ / / | |
;;  ___) |  __/ |_  | |_| | |_) | |  __/\ V /| | |
;; |____/ \___|\__|  \__,_| .__/   \___| \_/ |_|_|
;;                        |_|

(eval-when-compile (require 'use-package))

;; Set up basic evil, as early on as possible. Imagine being stuck without vim
;; bindings because something fails!!
(setq-default evil-want-keybinding nil) ; for evil-collection later
(setq-default evil-want-C-u-scroll t)
(setq-default evil-search-module 'evil-search)
(setq-default evil-ex-search-persistent-highlight nil)

(require 'evil)
(evil-mode 1)

;; Disable undo tree.
;; Seems like disabling it immediately no longer has any effect
(add-hook 'after-init-hook (defun my/disable-undo-tree ()
                             (global-undo-tree-mode -1)))

;;  _   _ _   _ _        ___    __     __         _       _     _
;; | | | | |_(_) |___   ( _ )   \ \   / /_ _ _ __(_) __ _| |__ | | ___  ___
;; | | | | __| | / __|  / _ \/\  \ \ / / _` | '__| |/ _` | '_ \| |/ _ \/ __|
;; | |_| | |_| | \__ \ | (_>  <   \ V / (_| | |  | | (_| | |_) | |  __/\__ \
;;  \___/ \__|_|_|___/  \___/\/    \_/ \__,_|_|  |_|\__,_|_.__/|_|\___||___/

;; Load necessary utils & device-specific options
(require 'my-utils)
(require 'my-devices)

;;   ____ _        _       _
;;  / ___| |_ _ __| |     | |_
;; | |   | __| '__| |_____| __|
;; | |___| |_| |  | |_____| |_
;;  \____|\__|_|  |_|      \__|

;; Swap C-t and C-x to make it easier on dvorak. This is applied
;; earliest possible and will work basically everywhere. It doesn't
;; work in all emacsclient sessions, unlike `key-translation-map', but
;; it does solve some rare issues with the latter alternative that
;; makes it worth battling the quirks.
(keyboard-translate ?\C-t ?\C-x)
(keyboard-translate ?\C-x ?\C-t)

;;  _   _ _       _                  _            _ _
;; | | | (_) __ _| |__    _ __  _ __(_) ___  _ __(_) |_ _   _
;; | |_| | |/ _` | '_ \  | '_ \| '__| |/ _ \| '__| | __| | | |
;; |  _  | | (_| | | | | | |_) | |  | | (_) | |  | | |_| |_| |
;; |_| |_|_|\__, |_| |_| | .__/|_|  |_|\___/|_|  |_|\__|\__, |
;;          |___/        |_|                            |___/

;; Full Evil load
(require 'my-evil)

;; File Navigation
(require 'my-navigation)

;; Ivy Selection
(require 'my-ivy)

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

;; Editing options
(setq-default c-basic-offset 4)
(setq-default fill-column 79)  ;; 80 exclusive
(setq-default indent-tabs-mode nil)
(setq-default require-final-newline t)
(setq-default tab-width 4)
(setq-default vc-follow-symlinks t)
(setq-default tags-add-tables nil)  ;; prevents super annoying popup that idk what it does

;; Enable auto-fill-mode in text-based buffers
(add-hook 'text-mode-hook (defun my/text-hook ()
                            (auto-fill-mode 1)))

;;  _   _                            _              _
;; | \ | | ___  _ __ _ __ ___   __ _| |  _ __  _ __(_) ___
;; |  \| |/ _ \| '__| '_ ` _ \ / _` | | | '_ \| '__| |/ _ \
;; | |\  | (_) | |  | | | | | | (_| | | | |_) | |  | | (_) |
;; |_| \_|\___/|_|  |_| |_| |_|\__,_|_| | .__/|_|  |_|\___/
;;                                      |_|

;; Editing tweaks, like loading editorconfig
(require 'my-editing)

;; Completion
(require 'my-completion)

;; Make _ a word character in all syntaxes, like it is in vim
(add-hook 'after-change-major-mode-hook
          (defun my/major-hook ()
            (modify-syntax-entry ?_ "w")))

;; Save list of open buffers...
(desktop-save-mode 1)

;; ... but clean the buffer list before opening
(add-hook 'desktop-after-read-hook
          (defun my/desktop-read-hook ()
            (clean-buffer-list)))

;; Beautiful styling
(require 'my-styling)

;; Automagically load environment
(use-package direnv
  :demand t
  :commands (direnv-mode)
  :config
  (direnv-mode 1))

;; Language-specific add-ons
(require 'my-languages)

;; Finally, load file templates
(require 'my-templates)

;;  _____     _          _ _
;; | ____|___| |__   ___| | |
;; |  _| / __| '_ \ / _ \ | |
;; | |___\__ \ | | |  __/ | |
;; |_____|___/_| |_|\___|_|_|

;; Enable mouse interaction
(xterm-mouse-mode 1)

;; Make Ctrl-d exit
(add-hook 'eshell-mode-hook
          (defun my/eshell-hook ()
            (evil-define-key 'insert eshell-mode-map (kbd "C-d")
              (lambda ()
                (interactive)
                (unless (eshell-send-eof-to-process)
                  (kill-buffer))))))

;;   ____                                          _
;;  / ___|___  _ __ ___  _ __ ___   __ _ _ __   __| |___
;; | |   / _ \| '_ ` _ \| '_ ` _ \ / _` | '_ \ / _` / __|
;; | |__| (_) | | | | | | | | | | | (_| | | | | (_| \__ \
;;  \____\___/|_| |_| |_|_| |_| |_|\__,_|_| |_|\__,_|___/

;; Packages required only for commands

(use-package gist
  :commands gist-region-private
  :init
  (defun gist (start end)
    "Send current region to a private gist"
    (interactive "r")
    (gist-region-private start end)))

(use-package uuid
  :commands uuid-string
  :init
  (defun uuid ()
    "Insert a new random UUID"
    (interactive)
    (insert (uuid-string))))

;; Eshell commands

(defun eshell/e (file)
  "Eshell alias to open a file inside emacs"
  (interactive)
  (find-file file))

;; Pure emacs commands

(defun touch ()
  "Touch the current file"
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

(defun load-dir ()
  "Load a directory"
  (interactive)
  (let* ((file-to-load (read-file-name "Load path: " "~/.emacs.d" nil t ""))
         (dir-to-load (file-name-directory file-to-load)))
    (if (not (file-regular-p file-to-load))
        (message "Not a regular file")
      (add-to-list 'load-path dir-to-load)
      (load file-to-load))))

;;  _____    _ _ _     ____
;; | ____|__| (_) |_  / ___|  ___ _ ____   _____ _ __
;; |  _| / _` | | __| \___ \ / _ \ '__\ \ / / _ \ '__|
;; | |__| (_| | | |_   ___) |  __/ |   \ V /  __/ |
;; |_____\__,_|_|\__| |____/ \___|_|    \_/ \___|_|

;; https://www.emacswiki.org/emacs/Edit_with_Emacs
(use-package edit-server
  :demand t
  :commands (edit-server-start)
  :config
  (setq edit-server-new-frame nil)
  (edit-server-start))

;;  _____
;; | ____|_ __ ___   __ _  ___ ___   ___  ___ _ ____   _____ _ __
;; |  _| | '_ ` _ \ / _` |/ __/ __| / __|/ _ \ '__\ \ / / _ \ '__|
;; | |___| | | | | | (_| | (__\__ \ \__ \  __/ |   \ V /  __/ |
;; |_____|_| |_| |_|\__,_|\___|___/ |___/\___|_|    \_/ \___|_|

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
