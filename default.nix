{ lib, symlinkJoin, makeWrapper, ranger, neovim, vimPlugins, vimUtils, fetchFromGitHub }:

let
  runtimeDeps = [
    ranger
  ];

  ranger-vim2 = vimUtils.buildVimPlugin {
    pname = "ranger-vim";
    version = "2019-10-30";
    src = fetchFromGitHub {
      owner = "francoiscabrol";
      repo = "ranger.vim";
      rev = "91e82debdf566dfaf47df3aef0a5fd823cedf41c";
      sha256 = "sha256-6ut7u6AwtyYbHLHa2jelf5PkbtlfHvuHfWRL5z1CTUQ=";
    };
  };

  nvim = neovim.override {
    configure = {
      customRC = builtins.readFile ./init.vim;

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
          ctrlp
          nerdtree
          ranger-vim2

          # VCS
          fugitive

          # Editing
          LanguageClient-neovim
          auto-pairs
          ncm2
          vim-surround

          # Languages
          vim-nix
        ];

        # Packages that might be lazy-loaded
        # with :packadd <name>
        opt = [];
      };
    };
  };
in symlinkJoin {
  name = "nvim";
  paths = [ nvim ];

  nativeBuildInputs = [ makeWrapper ];
  postBuild = ''
    rm "$out/bin/nvim"
    makeWrapper "${nvim}/bin/nvim" "$out/bin/nvim" --suffix PATH ':' ${lib.escapeShellArg (lib.makeBinPath runtimeDeps)}
  '';
}
