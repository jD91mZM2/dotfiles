;;; templates.el -*- lexical-binding: t; -*-

(set-file-templates!
 '("/\\.gitlab-ci\\.yml$" :trigger "__gitlab_ci_yml" :mode yaml-mode))
