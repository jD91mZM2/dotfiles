;;; templates.el -*- lexical-binding: t; -*-

(set-file-templates!
 '("/\\.gitlab-ci\\.yml$" :trigger "__gitlab_ci_yml" :mode yaml-mode)
 '("/flake\\.nix$" :trigger "__flake_nix" :mode nix-mode)
 '("\\.desktop$" :trigger "__desktop" :mode conf-desktop-mode)
 '("\\.tex$" :trigger "__tex" :mode latex-mode)
 '("\\.elm$" :trigger "__elm" :mode elm-mode))
