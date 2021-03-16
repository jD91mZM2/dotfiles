{
  imports = [

    # Base modules
    ../modules/base
    ../modules/console.nix
    ../modules/efi-boot.nix
    ../modules/gpg.nix
    ../modules/meta.nix
    ../modules/user
    ../modules/user/extra/syncthing.nix

    # Graphical packages
    ../modules/x11
    ../modules/x11/packages/firefox.nix
    ../modules/x11/packages/editing.nix
    ../modules/x11/packages/gaming.nix
    ../modules/x11/packages/media.nix
    ../modules/x11/packages/st.nix
    ../modules/x11/packages/thunderbird.nix

    # CLI Packages
    ../modules/packages/backup.nix
    ../modules/packages/convenience.nix
    ../modules/packages/git.nix
    ../modules/packages/inc.nix
    ../modules/packages/latex.nix
    ../modules/packages/neovim.nix
    ../modules/packages/programming/base.nix
    ../modules/packages/programming/go.nix
    ../modules/packages/programming/haskell.nix
    ../modules/packages/programming/nix.nix
    ../modules/packages/programming/python.nix
    ../modules/packages/programming/rust.nix
    ../modules/packages/virt.nix

  ];
}
