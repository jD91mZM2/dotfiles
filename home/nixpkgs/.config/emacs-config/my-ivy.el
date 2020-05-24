(eval-when-compile (require 'use-package))

(use-package ivy
  :demand t
  :config
  (ivy-mode 1))
(use-package counsel
  :after ivy
  :demand t
  :config
  (counsel-mode 1))

(provide 'my-ivy)
