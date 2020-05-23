(defvar my/option/transparency 100
  "The transparency of the Emacs window, in percent")
(defvar my/option/project-search-path '()
  "Becomes the value of projectile-project-search-path")

(eval-when-compile (require 'subr-x))

;; Per-system values
(let ((hostname (eval-when-compile (string-trim (shell-command-to-string "hostname")))))
  (cond ((equal hostname "samuel-computer")
         (setq my/option/transparency 90))
        ((equal hostname "samuel-laptop")
         (setq my/option/transparency 90)
         (setq my/option/project-search-path '("~/" "~/Coding/Rust/" "~/Coding/Rust/external/")))))

(provide 'my-devices)
