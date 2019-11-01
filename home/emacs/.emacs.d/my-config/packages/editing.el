(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1))

(use-package imenu-list
  :bind ("C-c i" . imenu-list-smart-toggle))

(use-package smartparens
  :demand t
  :commands sp-local-pairs
  :bind (("C-M-l" . sp-forward-slurp-sexp)
         ("C-M-h" . sp-forward-barf-sexp))
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)

  ;; Allow using smartparens from minibuffer
  (setq sp-ignore-modes-list (remove 'minibuffer-inactive-mode sp-ignore-modes-list))

  ;; Configure evil to use smartparens for %
  (evil-define-motion my/matching-paren (num)
    :type inclusive
    (let* ((expr (sp-get-paired-expression))
           (begin (plist-get expr :beg))
           (next (plist-get expr :end))
           (end (if next (- next 1) nil)))
      (if (eq (point) end)
          (goto-char begin)
        (when end (goto-char end)))))
  (evil-global-set-key 'motion (kbd "%") 'my/matching-paren)

  ;; Create double newline on enter
  (defun my/newline-indent (&rest _ignored)
    "Call when newline is pressed - this will only add one newline"
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))
  (sp-local-pair 'prog-mode "{" nil :post-handlers '((my/newline-indent "RET")))
  (sp-local-pair 'prog-mode "(" nil :post-handlers '((my/newline-indent "RET")))
  (sp-local-pair 'prog-mode "[" nil :post-handlers '((my/newline-indent "RET"))))

(use-package string-inflection
  :after hydra
  :config
  (defhydra my/string-inflection-keys (global-map "C-c")
    "
Toggle string casing
--------------------
[_s_]: snake%(quote _)case
[_S_]: SCREAMING%(quote _)SNAKE%(quote _)CASE
[_k_]: kebab-case
[_c_]: camelCase
[_C_]: PascalCase
"
    ("_" string-inflection-cycle "Cycle common")
    ("-" string-inflection-all-cycle "Cycle all" :bind nil)
    ("s" string-inflection-underscore :bind nil)
    ("S" string-inflection-upcase :bind nil)
    ("k" string-inflection-kebab-case :bind nil)
    ("c" string-inflection-lower-camelcase :bind nil)
    ("C" string-inflection-camelcase :bind nil)))
