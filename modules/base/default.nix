{ pkgs, ... }:

{
  imports = [
    ./shell.nix
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

  # Base packages
  environment.systemPackages = with pkgs; [
    # Git
    git
    git-lfs

    # C compiler utilities
    binutils

    # zip/unzip
    zip
    unzip

    # Must have CLI applications
    bc
    htop
    mosh
    ncdu
    tmux
    trash-cli

    # Must have CLI utils
    file
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
}
