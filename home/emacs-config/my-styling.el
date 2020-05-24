(eval-when-compile (require 'use-package))

;; Show trailing whitespace everywhere except in terminals
(setq-default show-trailing-whitespace t)

(add-hook 'term-mode-hook
          (defun my/term-hook ()
            (setq-local show-trailing-whitespace nil)))

;; Display line numbers
(setq-default display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)
;; Display current line
(global-hl-line-mode 1)

;; Display errnous whitespace
(setq-default whitespace-style '(face space-before-tab::tab))
(global-whitespace-mode 1)

;; Disable UI components
(blink-cursor-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Fonts
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

;; Transparency!
(add-to-list 'default-frame-alist (cons 'alpha my/option/transparency)) ; default frame settings
(set-frame-parameter (selected-frame) 'alpha my/option/transparency)    ; for current session

;; Dracula theme...
(use-package dracula-theme
  :demand t
  :config
  (load-theme 'dracula t))

;; ... with a powerline modeline
(require 'powerline)
(defalias 'my/modeline 'powerline-center-evil-theme)
(my/modeline)

;; ... and smooth scroll
(use-package sublimity
  :config
  (require 'sublimity-scroll)
  (sublimity-mode 1))

(provide 'my-styling)
