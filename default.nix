{ lib, symlinkJoin, makeWrapper, ranger, neovim, vimPlugins }:

let
  runtimeDeps = [
    ranger
  ];

  nvim = neovim.override {
    configure = {
      customRC = builtins.readFile ./init.vim;

      packages.nixPackages = with vimPlugins; {
        # Required packages
        start = [
          # Style
          dracula-vim
          vim-airline
          vim-airline-themes

          # Editing
          vim-surround
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
