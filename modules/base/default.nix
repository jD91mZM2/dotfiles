{ pkgs, ... }:

{
  imports = [
    ./shell.nix
    ./tmux.nix
  ];

  # Set default locale
  i18n.defaultLocale = "en_GB.UTF-8";

  # Set timezone
  time.timeZone = "Europe/Stockholm";

  # Nix flakes
  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  # Clean /tmp
  boot.cleanTmpDir = true;

  # Base packages
  environment.systemPackages = with pkgs; [
    # Git
    git
    git-lfs

    # C compiler utilities
    binutils
    gcc

    # zip/unzip
    zip
    unzip

    # Must have CLI applications
    bc
    htop
    mosh
    ncdu
    trash-cli

    # Must have CLI utils
    file
    jq
    patchelf
    pciutils
    pv
    tree
    wget

    # Better versions of coreutils
    bat
    exa
    fd
    ripgrep
  ];

  home = {
    home.file.".editorconfig".source = ./default.editorconfig;
  };
}
