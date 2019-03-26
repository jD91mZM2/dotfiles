{ config, pkgs, ... }:

let
  abottomod = (pkgs.callPackage ./pypi.nix {}) {
    name = "abottomod";
    src = ~/Coding/Python/abottomod;
  };
  timeywimey = (pkgs.callPackage ./pypi.nix {}) {
    name = "timeywimey";
    src = ~/Coding/Python/timeywimey;
  };

  mcbotface = (pkgs.callPackage ./rust.nix {}) {
    name = "mcbotface";
    src = ~/Coding/Rust/mcbotface;
    buildInputs = with pkgs; [ pkgconfig openssl sqlite ];
  };
  redox-world-map = (pkgs.callPackage ./rust.nix {}) {
    name = "redox-world-map";
    src = ~/Coding/Web/redox-world-map;
    buildInputs = with pkgs; [ pkgconfig openssl sqlite ];
    wrapperHook = ''
      ln -s $out/src/Rocket.toml . || true
    '';
  };

  utils = pkgs.callPackage ./utils.nix {};
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
  security.sudo.wheelNeedsPassword = false;
  users.defaultUserShell = pkgs.zsh;
  users.users.user = {
    createHome = true;
    home = "/home/user";
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keyFiles = [ ~/.ssh/id_ed25519.pub ];
  };

  # Discord bots
  users.users.abottomod = {
    createHome = true;
    home = "/var/lib/abottomod";
  };
  systemd.services.abottomod = {
    description = "A bot to moderate my discord server";
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
    description = "TimeyWimey discord bot";
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
    description = "Mcbotface discord bot";
    script = "${mcbotface}/bin/start";
    serviceConfig = {
      User = "mcbotface";
      WorkingDirectory = "/var/lib/mcbotface";
    };
    wantedBy = [ "multi-user.target" ];
  };

  users.users.redox-world-map = {
    createHome = true;
    home = "/var/lib/redox-world-map";
  };
  systemd.services.redox-world-map = {
    description = "Redox World Map";
    script = "${redox-world-map}/bin/start";
    serviceConfig = {
      User = "redox-world-map";
      WorkingDirectory = "/var/lib/redox-world-map";
    };
    wantedBy = [ "multi-user.target" ];
  };
}
