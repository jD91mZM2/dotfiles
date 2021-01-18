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

    ../modules/packages/media.nix
    ../modules/packages/programming/nix.nix
  ];
}
