{
  imports = [
    ./generic
  ];

  setup = {
    source = ./full.nix;

    env.enable = true;

    shells.enableZsh = true;
    shells.enableBash = true;
    shells.enableGit = true;
    shells.personal = true;
  };
}
