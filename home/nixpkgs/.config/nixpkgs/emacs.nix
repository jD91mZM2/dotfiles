{ pkgs, ...}:

let
  packages = (epkgs:
    [(epkgs.trivialBuild {
      pname = "mcf-mode";
      version = "git";
      src = pkgs.fetchFromGitHub {
        owner = "rasensuihei";
        repo = "mcf";
        rev = "7fe4c74a47b1820806bf92d818039dafb2df114b";
        sha256 = "1s5an8i95q4zpk7l1ppk639ibq9iglb363bxz2pnd22m6rvmysxf";
      };
    })]
    ++
    (with epkgs.elpaPackages; [
      auctex
      org
    ])
    ++
    (with epkgs.melpaPackages; [
      aggressive-indent
      auto-compile
      company
      company-auctex
      company-lsp
      company-math
      counsel
      dhall-mode
      use-package
      direnv
      dockerfile-mode
      dracula-theme
      edit-indirect
      edit-server
      editorconfig
      evil
      evil-args
      evil-collection
      evil-easymotion
      evil-magit
      evil-surround
      flycheck
      gist
      go-mode
      graphviz-dot-mode
      htmlize
      hydra
      imenu-list
      ivy
      lsp-mode
      lsp-ui
      markdown-mode
      nasm-mode
      nix-mode
      ob-rust
      org-present
      powerline
      projectile
      projectile-ripgrep
      ranger
      rust-playground
      rustic
      slime
      slime-company
      smartparens
      string-inflection
      sublimity
      uuid
      web-mode
      yaml-mode
      yasnippet
    ])
  );
  emacsConf = "${pkgs.emacsPackages.trivialBuild {
    pname = "my-config";
    version = "local";
    packageRequires = packages pkgs.emacsPackages;
    src = ../emacs-config;
  }}/share/emacs/site-lisp";
in
{
  home.file.".emacs.d/init.elc".source = "${emacsConf}/init.elc";
  home.file.".emacs.d/my-config".source = "${emacsConf}";

  programs.emacs = {
    enable = true;

    extraPackages = packages;
  };
}
