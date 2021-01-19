{
  imports = [
    ../modules/base
    ../modules/console.nix
    ../modules/efi-boot.nix
    ../modules/gpg.nix
    ../modules/meta.nix
    ../modules/neovim.nix
    ../modules/user
    ../modules/x11

    # Graphical packages
    ../modules/x11/packages/chromium.nix
    ../modules/x11/packages/media.nix
    ../modules/x11/packages/st.nix
    ../modules/x11/packages/thunderbird.nix

    # CLI Packages
    ../modules/packages/git.nix
    ../modules/packages/programming/nix.nix
  ];
}
