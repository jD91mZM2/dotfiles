{ lib, symlinkJoin, makeWrapper, pkgs, neovim, neovim-remote, vimPlugins, vimUtils, fetchFromGitHub }:

let
  runtimeDeps = with pkgs; [
    fd
    fzf
    ranger
    ripgrep
  ];

  ranger-vim2 = vimUtils.buildVimPlugin {
    pname   = "ranger-vim";
    version = "2019-10-30";
    src = fetchFromGitHub {
      owner  = "francoiscabrol";
      repo   = "ranger.vim";
      rev    = "91e82debdf566dfaf47df3aef0a5fd823cedf41c";
      sha256 = "sha256-6ut7u6AwtyYbHLHa2jelf5PkbtlfHvuHfWRL5z1CTUQ=";
    };
  };

  nvim = neovim.override {
    configure = {
      customRC = ''
        source ${./.}/init.vim
      '';

      packages.nixPackages = with vimPlugins; {
        # Required packages
        start = [
          # Dependencies
          nvim-yarp # ncm2

          # Libraries
          vim-operator-user

          # Style
          dracula-vim
          vim-airline
          vim-airline-themes

          # Navigation
          fzf-vim
          nerdtree
          ranger-vim2
          vim-rooter

          # VCS
          fugitive

          # Editing
          auto-pairs
          ncm2
          ncm2-ultisnips
          tabular
          ultisnips
          vim-commentary
          vim-exchange
          vim-repeat
          vim-surround

          # Validation
          LanguageClient-neovim

          # Languages
          vim-nix
          vim-toml
          vim-markdown
        ];

        # Packages that might be lazy-loaded
        # with :packadd <name>
        opt = [];
      };
    };
  };
in symlinkJoin {
  name  = "nvim";
  paths = [ nvim neovim-remote ];

  nativeBuildInputs = [ makeWrapper ];
  postBuild = ''
    # Add runtime deps to neovim
    rm "$out/bin/nvim"
    makeWrapper "${nvim}/bin/nvim" "$out/bin/nvim" --suffix PATH ':' ${lib.escapeShellArg (lib.makeBinPath runtimeDeps)}

    # Alias neovim-remote as "e" (for edit)
    makeWrapper "${neovim-remote}/bin/nvr" "$out/bin/e" --add-flags -s
  '';
}
