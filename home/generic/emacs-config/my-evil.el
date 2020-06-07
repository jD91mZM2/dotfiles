(eval-when-compile (require 'use-package))
(require 'bind-key)
(require 'evil)

;;  _   _      _                    __                  _   _
;; | | | | ___| |_ __   ___ _ __   / _|_   _ _ __   ___| |_(_) ___  _ __  ___
;; | |_| |/ _ \ | '_ \ / _ \ '__| | |_| | | | '_ \ / __| __| |/ _ \| '_ \/ __|
;; |  _  |  __/ | |_) |  __/ |    |  _| |_| | | | | (__| |_| | (_) | | | \__ \
;; |_| |_|\___|_| .__/ \___|_|    |_|  \__,_|_| |_|\___|\__|_|\___/|_| |_|___/
;;              |_|

(defun my/with-inverted-case (beg end inner)
  ;; Invert casing
  (my/util/replace-region beg end (my/util/invert-case (buffer-substring-no-properties beg end)))
  ;; Execute inner function
  (funcall inner)
  ;; Invert casing back
  (my/util/replace-region beg end (my/util/invert-case (buffer-substring-no-properties beg end))))

;;  _  __          _     _           _ _
;; | |/ /___ _   _| |__ (_)_ __   __| (_)_ __   __ _ ___
;; | ' // _ \ | | | '_ \| | '_ \ / _` | | '_ \ / _` / __|
;; | . \  __/ |_| | |_) | | | | | (_| | | | | | (_| \__ \
;; |_|\_\___|\__, |_.__/|_|_| |_|\__,_|_|_| |_|\__, |___/
;;           |___/                             |___/

;; ASCII headers
(evil-global-set-key 'normal (kbd "gca")
                     (evil-define-operator my/figlet (beg end)
                       (shell-command-on-region beg end "figlet" (current-buffer) t)

                       (whitespace-cleanup-region (region-beginning) (region-end))
                       (comment-region (region-beginning) (region-end))
                       (indent-region-line-by-line (region-beginning) (region-end))))

;; Sort stuff
(evil-global-set-key 'normal (kbd "gss")
                     (evil-define-operator my/sort-lines (beg end)
                       (my/with-inverted-case beg end (lambda ()
                                                        (sort-lines nil beg end)))))
(evil-global-set-key 'normal (kbd "gs,")
                     (evil-define-operator my/sort-fields (beg end)
                       (my/with-inverted-case beg end (lambda ()
                                                        (sort-regexp-fields nil "[^,[:space:]\n]+" "\\&" beg end)))))

;; Toggle comment
(evil-global-set-key 'normal (kbd "gcc")
                     (evil-define-operator my/comment (beg end)
                       (comment-or-uncomment-region beg end)))

;; Alignment stuff
(defmacro my/define-align (key function regexp docstring)
  `(evil-global-set-key 'normal (kbd ,(concat "g=" key))
                        (evil-define-operator ,function (beg end)
                          ,docstring
                          (align-regexp beg end ,regexp nil nil t))))
(my/define-align "=" my/align-symbols "\\(\\s-*\\)\\(=\\|?\\)"
                 "Align equal marks")
(my/define-align "#" my/align-comments "\\(\\s-+\\)\\(#\\|//\\|/\\*\\|;\\)"
                 "Align all comments")
(my/define-align "," my/align-comma ",\\(\\s-*\\)[^[:space:]\n]"
                 "Align all non-whitespace characters after a comma")
(my/define-align ":" my/align-colon ":\\(\\s-+\\)[^[:space:]\n]"
                 "Align all non-whitespace characters after a colon")
(my/define-align " SPC" my/align-word "[^[:space:]\n]\\(\\s-+\\)\\s-[^[:space:]\n]"
                 "Align all non-whitespace characters preceeded by at least 2 spaces")


;; Other useful shorthands
(evil-global-set-key 'normal (kbd "gt") 'switch-to-buffer)
(evil-global-set-key 'normal (kbd "gcw") 'delete-trailing-whitespace)
(evil-global-set-key 'normal (kbd "D") (lambda ()
                                         (interactive)
                                         (beginning-of-line)
                                         (kill-line)))
(evil-global-set-key 'normal (kbd "gyf") (lambda ()
                                           (interactive)
                                           (kill-new (buffer-file-name))
                                           (message "%s" (buffer-file-name))))

;; Insert mode shortcuts
(evil-global-set-key 'insert (kbd "C-c d")
                     (lambda (prefix)
                       (interactive "P")
                       (insert (format-time-string
                                (if prefix
                                    (let* ((seconds (car (current-time-zone)))
                                           (minutes (/ seconds 60))
                                           (hours   (/ minutes 60)))
                                      (concat "%FT%T" (format "%+.2d:%.2d" hours (% minutes 60))))
                                  "%F")))))
(evil-global-set-key 'insert (kbd "C-c n")
                     (lambda (num)
                       (interactive "nInput start number: ")
                       (kmacro-set-counter num)))
(evil-global-set-key 'normal (kbd "C-c b") 'bookmark-bmenu-list)
(evil-global-set-key 'normal (kbd "C-c c") 'recompile)
(evil-global-set-key 'normal (kbd "C-c s") 'eshell)

;;  _____       _ _             _                 _
;; | ____|_   _(_) |   _____  _| |_ ___ _ __  ___(_) ___  _ __  ___
;; |  _| \ \ / / | |  / _ \ \/ / __/ _ \ '_ \/ __| |/ _ \| '_ \/ __|
;; | |___ \ V /| | | |  __/>  <| ||  __/ | | \__ \ | (_) | | | \__ \
;; |_____| \_/ |_|_|  \___/_/\_\\__\___|_| |_|___/_|\___/|_| |_|___/

(use-package evil-collection
  :custom
  (evil-collection-company-use-tng nil)
  :config
  (evil-collection-init))

(use-package evil-args
  :config
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

  (defun my/transpose-args (direction)
    "Interchange the current argument with the one in direction,
leaving the point at the end of the latter"
    (destructuring-bind (beg1 end1 _) (evil-inner-arg)
      ;; Get the text of the first argument
      (let ((text1 (buffer-substring-no-properties beg1 end1)))
        (apply direction '(1))

        ;; Get the text of the second argument
        (let ((text2 (destructuring-bind (beg2 end2 _) (evil-inner-arg)
                       (buffer-substring-no-properties beg2 end2))))

          ;; Replace the first
          (my/util/replace-region beg1 end1 text2)

          ;; Re-obtain text (because marks probably changed) and replace second
          (destructuring-bind (beg2 end2 _) (evil-inner-arg)
            (my/util/replace-region beg2 end2 text1))))))

  (evil-define-key 'normal prog-mode-map (kbd "M-n") (lambda () (interactive) (my/transpose-args 'evil-forward-arg)))
  (evil-define-key 'normal prog-mode-map (kbd "M-p") (lambda () (interactive) (my/transpose-args 'evil-backward-arg))))

(use-package evil-magit
  :bind (("C-c g" . magit-status))
  :demand t)
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;;  _   _         _     _       _     _ _       _     _
;; | \ | | ___   | |__ (_) __ _| |__ | (_) __ _| |__ | |_
;; |  \| |/ _ \  | '_ \| |/ _` | '_ \| | |/ _` | '_ \| __|
;; | |\  | (_) | | | | | | (_| | | | | | | (_| | | | | |_
;; |_| \_|\___/  |_| |_|_|\__, |_| |_|_|_|\__, |_| |_|\__|
;;                        |___/           |___/

(defvar my/stop-hl-timer-last nil)

(defun my/stop-hl-timer (_)
  ;; Cancel previous timer
  (when my/stop-hl-timer-last
    (cancel-timer my/stop-hl-timer-last))
  ;; Create new timer
  (setq my/stop-hl-timer-last
        (run-at-time 1 nil (lambda () (evil-ex-nohighlight)))))

(advice-add 'evil-ex-search-activate-highlight :after 'my/stop-hl-timer)

(provide 'my-evil)
