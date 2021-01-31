{ pkgs, lib, ... }:
with lib;
{
  environment.systemPackages = with pkgs.python3Packages; [
    # Python
    python

    # Editor stuff
    python-language-server
    black

    # Type checking
    mypy

    # Project management
    poetry
  ];

  # Black conflicts with pycodestyle. See
  # https://github.com/psf/black/blob/b55fb821e795bc45903e6f7b89e8857ddb5a94f8/docs/compatible_configs.md#flake8
  home.xdg.configFile."flake8".text = ''
    [flake8]
    max-line-length = 88
    ignore = E203
  '';
}
