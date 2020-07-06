;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;;  ____       _   _   _
;; / ___|  ___| |_| |_(_)_ __   __ _ ___
;; \___ \ / _ \ __| __| | '_ \ / _` / __|
;;  ___) |  __/ |_| |_| | | | | (_| \__ \
;; |____/ \___|\__|\__|_|_| |_|\__, |___/
;;                             |___/

(setq-default

 ;; Some functionality uses this to identify you, e.g. GPG configuration, email
 ;; clients, file templates and snippets.
 user-full-name "jD91mZM2"
 user-mail-address "me@krake.one"

 ;; My preferred theme
 doom-theme 'doom-dracula

 ;; If you use `org' and don't want your org files in the default location below,
 ;; change `org-directory'. It must be set before org loads!
 org-directory "~/Sync/Documents"

 ;; This determines the style of line numbers in effect. If set to `nil', line
 ;; numbers are disabled. For relative line numbers, set this to `relative'.
 display-line-numbers-type 'relative

 ;; This is annoying
 +evil-want-o/O-to-continue-comments nil

 ;; Use rust-analyzer for rust server
 rustic-lsp-server 'rust-analyzer)

;; Swap C-x and C-t
(keyboard-translate ?\C-x ?\C-t)
(keyboard-translate ?\C-t ?\C-x)

;; Show trailing whitespace for certain modes
(setq-hook! ('prog-mode-hook 'conf-mode-hook)
  show-trailing-whitespace t)

(add-hook! 'prog-mode-hook
  (defun my/prog-mode-hook ()
    (modify-syntax-entry ?\_ "w")))

;;  _   _      _                    __                  _   _
;; | | | | ___| |_ __   ___ _ __   / _|_   _ _ __   ___| |_(_) ___  _ __  ___
;; | |_| |/ _ \ | '_ \ / _ \ '__| | |_| | | | '_ \ / __| __| |/ _ \| '_ \/ __|
;; |  _  |  __/ | |_) |  __/ |    |  _| |_| | | | | (__| |_| | (_) | | | \__ \
;; |_| |_|\___|_| .__/ \___|_|    |_|  \__,_|_| |_|\___|\__|_|\___/|_| |_|___/
;;              |_|

(require 'cl-lib)

(defun my/replace-region (beg end text)
  "Replace a region of text in the current buffer"
  (delete-region beg end)
  (save-excursion
    (goto-char beg)
    (insert text)))

(defun my/invert-case (string)
  "Return string but with the casing inverted. \"Hello\" => \"hELLO\""
  (cl-map 'string
          (lambda (c)
            (if (or (<= ?a c ?z) (<= ?A c ?Z))
                (logxor c 32)
              c))
          string))

(defun my/with-inverted-case (beg end inner)
  ;; Invert casing
  (my/replace-region beg end (my/invert-case (buffer-substring-no-properties beg end)))
  ;; Execute inner function
  (funcall inner)
  ;; Invert casing back
  (my/replace-region beg end (my/invert-case (buffer-substring-no-properties beg end))))

(defvar my/font-lock-additions (make-hash-table))
(defun my/font-lock-extend (mode keywords)
  (let ((prev (gethash mode my/font-lock-additions)))
    (when prev
      (font-lock-remove-keywords mode prev))
    (font-lock-add-keywords mode keywords)
    (puthash mode keywords my/font-lock-additions)))

;;  _  __          _     _           _ _
;; | |/ /___ _   _| |__ (_)_ __   __| (_)_ __   __ _ ___
;; | ' // _ \ | | | '_ \| | '_ \ / _` | | '_ \ / _` / __|
;; | . \  __/ |_| | |_) | | | | | (_| | | | | | (_| \__ \
;; |_|\_\___|\__, |_.__/|_|_| |_|\__,_|_|_| |_|\__, |___/
;;           |___/                             |___/

(map!
 :n "g t" 'switch-to-buffer

 ;; ASCII headers
 :i "C-c a" (defun my/figlet (input)
              (interactive "MInput: ")
              (let ((big (with-temp-buffer
                           (insert input)
                           (shell-command-on-region (point-min) (point-max) "figlet" t t)
                           (whitespace-cleanup-region (point-min) (point-max))
                           (comment-region (point-min) (point-max))
                           (buffer-string))))
                (insert big)
                (indent-region-line-by-line (- (point) (length big)) (point))))

 ;; Sort stuff
 :n "g s s" (evil-define-operator my/sort-lines (beg end)
              (my/with-inverted-case beg end (lambda ()
                                               (sort-lines nil beg end))))
 :n "g s ," (evil-define-operator my/sort-fields (beg end)
              (my/with-inverted-case beg end (lambda ()
                                               (sort-regexp-fields nil "[[:space:]]*\\([^,[:space:]\n][^,\n]+\\)" "\\1" beg end))))

 ;; Kill line
 :n "D" (lambda ()
          (interactive)
          (beginning-of-line)
          (kill-line)))

(after! ranger (map! :n "C-c r" 'ranger))

;;  __  __           _ _  __ _           _   _
;; |  \/  | ___   __| (_)/ _(_) ___ __ _| |_(_) ___  _ __  ___
;; | |\/| |/ _ \ / _` | | |_| |/ __/ _` | __| |/ _ \| '_ \/ __|
;; | |  | | (_) | (_| | |  _| | (_| (_| | |_| | (_) | | | \__ \
;; |_|  |_|\___/ \__,_|_|_| |_|\___\__,_|\__|_|\___/|_| |_|___/

;; I actually somewhat like Emacs' undo system :(
(after! undo-tree (global-undo-tree-mode -1))

;; Breaks bit-shifting
(after! rustic (setq! rustic-match-angle-brackets nil))

;; Replace assembler mode with nasm-mode
(after! nasm-mode (setq! nasm-after-mnemonic-whitespace :space))
(add-hook! 'asm-mode-hook
  (defun my/asm-mode-hook ()
    (nasm-mode)
    ;; aggressive-indent-mode does not work well with assembly
    (aggressive-indent-mode -1)))

(after! yassnippet (pushnew! yas-snippet-dirs (expand-file-name "snippets" (dir!))))

;;  __  __        _                                  _
;; |  \/  | __ _ (_) ___  _ __   _ __ ___   ___   __| | ___  ___
;; | |\/| |/ _` || |/ _ \| '__| | '_ ` _ \ / _ \ / _` |/ _ \/ __|
;; | |  | | (_| || | (_) | |    | | | | | | (_) | (_| |  __/\__ \
;; |_|  |_|\__,_|/ |\___/|_|    |_| |_| |_|\___/ \__,_|\___||___/
;;             |__/

(use-package! aggressive-indent
  :config
  (global-aggressive-indent-mode 1))
(use-package! mcf-mode
  :mode "\\.mcfunction\\'")

;;   ____                                          _
;;  / ___|___  _ __ ___  _ __ ___   __ _ _ __   __| |___
;; | |   / _ \| '_ ` _ \| '_ ` _ \ / _` | '_ \ / _` / __|
;; | |__| (_) | | | | | | | | | | | (_| | | | | (_| \__ \
;;  \____\___/|_| |_| |_|_| |_| |_|\__,_|_| |_|\__,_|___/

(defun touch ()
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

(defun date (prefix)
  (interactive "P")
  (insert (format-time-string
           (if prefix
               (let* ((seconds (car (current-time-zone)))
                      (minutes (/ seconds 60))
                      (hours   (/ minutes 60)))
                 (concat "%FT%T" (format "%+.2d:%.2d" hours (% minutes 60))))
             "%F"))))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
