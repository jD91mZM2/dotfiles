rec {
  email = "me@krake.one";
  name = "jD91mZM2";

  user = "user";
  home = "/home/${user}";
  dotfiles = "${home}/dotfiles";
  sshKeys = [ ~/.ssh/id_ed25519.pub ];

  secret = import ./secret.nix;
}
