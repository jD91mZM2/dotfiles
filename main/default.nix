{ config, pkgs, ... }:

let
  abottomod = pkgs.callPackage (import ./pypi.nix "abottomod" ~/Coding/Python/abottomod) {};
  timeywimey = pkgs.callPackage (import ./pypi.nix "timeywimey" ~/Coding/Python/timeywimey) {};

  mcbotface = pkgs.callPackage ./mcbotface.nix {};
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

  # Discord bots
  users.users.abottomod = {
    createHome = true;
    home = "/var/lib/abottomod";
  };
  systemd.services.abottomod = {
    description = "A bot to moderate my server";
    script = "${abottomod}/bin/start";
    serviceConfig = {
      User = "abottomod";
      WorkingDirectory = "/var/lib/abottomod";
    };
    wantedBy = [ "multi-user.target" ];
  };
  users.users.timeywimey = {
    createHome = true;
    home = "/var/lib/timeywimey";
  };
  systemd.services.timeywimey = {
    description = "The discord bot known as TimeyWimey";
    script = "${timeywimey}/bin/start";
    serviceConfig = {
      User = "timeywimey";
      WorkingDirectory = "/var/lib/timeywimey";
    };
    wantedBy = [ "multi-user.target" ];
  };
  users.users.mcbotface = {
    createHome = true;
    home = "/var/lib/mcbotface";
  };
  systemd.services.mcbotface = {
    description = "Another private discord bot";
    script = "${mcbotface}/bin/mcbotface";
    serviceConfig = {
      User = "mcbotface";
      WorkingDirectory = "/var/lib/mcbotface";
    };
    wantedBy = [ "multi-user.target" ];
  };
}
