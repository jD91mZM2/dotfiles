{ pkgs, ... }:

let
  outLispPath = "share/emacs/site-lisp";
in
rec {
  configDir = "${pkgs.emacsPackages.trivialBuild {
    pname = "my-config";
    version = "local";
    packageRequires = depsForEpkgs pkgs.emacsPackages;
    src = ../emacs-config;

    postPatch = ''
      mv init.el realinit.el
      echo "(add-to-list 'load-path \"$out/${outLispPath}\" t)" >> init.el
      cat realinit.el >> init.el
      rm realinit.el
    '';
    postInstall = ''
      cp -r templates "$out/${outLispPath}/templates"
      cp -r snippets "$out/${outLispPath}/snippets"
    '';
  }}/${outLispPath}";

  package = pkgs.emacsWithPackages depsForEpkgs;

  depsForEpkgs = (epkgs:
    [
      (epkgs.trivialBuild {
        pname = "mcf-mode";
        version = "git";
        src = pkgs.fetchFromGitHub {
          owner = "rasensuihei";
          repo = "mcf";
          rev = "7fe4c74a47b1820806bf92d818039dafb2df114b";
          sha256 = "1s5an8i95q4zpk7l1ppk639ibq9iglb363bxz2pnd22m6rvmysxf";
        };
      })
    ]
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
      direnv
      dockerfile-mode
      dracula-theme
      edit-indirect
      edit-server
      editorconfig
      evil
      evil-args
      evil-collection
      evil-magit
      evil-surround
      flycheck
      gist
      go-mode
      graphviz-dot-mode
      haskell-mode
      htmlize
      hydra
      imenu-list
      ivy
      lsp-mode
      lsp-ui
      lua-mode
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
      use-package
      uuid
      web-mode
      yaml-mode
      yasnippet
    ])
  );
}
