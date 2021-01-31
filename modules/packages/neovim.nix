{ pkgs, self, system, ... }:
let
  neovim = self.packages."${system}".neovim;
in
{
  # Add the package
  environment.systemPackages = [
    # Install the main package
    neovim

    # Install vimscript linter
    pkgs.vim-vint
  ];

  # Set as default editor
  environment.sessionVariables = {
    EDITOR = "e-wait";
    VISUAL = "e-wait";
  };
}
