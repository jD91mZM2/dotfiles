{ config, lib, pkgs, ... }:

{
  # Packages
  environment.systemPackages = with pkgs; [
    # Command line
    tmux
    neovim # nice to have as backup if emacs would break

    # Must have utils
    bind
    binutils
    cryptsetup
    efibootmgr
    gist
    httpie
    manpages
    mosh
    nix-index
    patchelf
    pciutils
    socat
    unzip
    wget
    zip
  ];
}
