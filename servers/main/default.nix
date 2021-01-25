(import ../..).lib.makeModule "x86_64-linux" {
  imports = [
    ./minimal.nix

    ../../modules/base
    ../../modules/user

    ../../modules/packages/neovim.nix
  ];

  system.stateVersion = "21.03";
  home.home.stateVersion = "21.03";
}
