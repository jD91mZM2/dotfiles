{ config, pkgs, ... }:

let
  abottomod = pkgs.callPackage (import ./pypi.nix "abottomod" ~/Coding/Python/abottomod) {};
  timeywimey = pkgs.callPackage (import ./pypi.nix "timeywimey" ~/Coding/Python/timeywimey) {};
in {
  # Deployment metadata
  deployment = {
    targetEnv = "digitalOcean";
    digitalOcean = {
      region = "ams3";
      size = "s-1vcpu-1gb";
    };
  };

  # System config
  programs.mosh.enable = true;
  programs.zsh = {
    enable = true;
    autosuggestions.enable = true;
    promptInit = ''
      powerline() {
        PS1="$(${pkgs.powerline-rs}/bin/powerline-rs --shell zsh "$?")"
      }
      precmd_functions+=(powerline)
    '';
    interactiveShellInit = ''
      . ${pkgs.grml-zsh-config}/etc/zsh/zshrc
    '';
    syntaxHighlighting.enable = true;
  };

  # General purpose users
  users.defaultUserShell = pkgs.zsh;
  users.users.user = {
    createHome = true;
    home = "/home/user";
    isNormalUser = true;
    openssh.authorizedKeys.keyFiles = [ ~/.ssh/id_ed25519.pub ];
  };

  # User for python applications
  users.users.happy = {
    createHome = true;
    home = "/home/hapPy";
  };
  systemd.services.abottomod = {
    description = "A bot to moderate my server";
    script = "${abottomod}/bin/start";
    serviceConfig = {
      User = "happy";
    };
    wantedBy = [ "multi-user.target" ];
  };
  systemd.services.timeywimey = {
    description = "The discord bot known as TimeyWimey";
    script = "${timeywimey}/bin/start";
    serviceConfig = {
      User = "happy";
    };
    wantedBy = [ "multi-user.target" ];
  };
}
