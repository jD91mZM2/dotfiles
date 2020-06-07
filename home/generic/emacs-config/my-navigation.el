(eval-when-compile (require 'use-package))
(require 'bind-key)

(use-package projectile
  :after projectile-ripgrep
  :demand t
  :custom
  (projectile-completion-system 'ivy)
  (projectile-project-search-path my/option/project-search-path)
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
(use-package projectile-ripgrep)

(use-package ranger
  :bind ("C-c r" . ranger)
  :custom
  (ranger-override-dired 'ranger)
  (ranger-override-dired-mode t))

(provide 'my-navigation)
