{ pkgs, ... }:

let
  config = import ./config.nix;

  # Packages
  abottomod = (pkgs.callPackage ./pypi.nix {}) {
    name = "abottomod";
    src = ~/Coding/Python/abottomod;
  };
  timeywimey = (pkgs.callPackage ./pypi.nix {}) {
    name = "timeywimey";
    src = ~/Coding/Python/timeywimey;
  };
  redox-world-map = (pkgs.callPackage ./rust.nix {}) {
    name = "redox-world-map";
    src = ~/Coding/Web/redox-world-map;
    buildInputs = with pkgs; [ pkgconfig openssl sqlite ];
    wrapperHook = ''
      ln -sf $out/src/Rocket.toml .
    '';
  };

  # Helpers
  createServiceUser = { name, script }: {
    users.users."${name}" = {
      createHome = true;
      home = "/var/lib/${name}";
    };
    systemd.services."${name}" = {
      inherit script;
      serviceConfig = {
        User = name;
        WorkingDirectory = "/var/lib/${name}";
        Restart = "always";
        RestartSec = "10s";
      };
      wantedBy = [ "multi-user.target" ];
    };
  };
  createZncServers = servers: builtins.listToAttrs (map (server: let
      url = builtins.getAttr server servers;
    in {
      name = server;
      value = {
        server = url;
        modules = [ "simple_away" "sasl" ];
      };
    }) (builtins.attrNames servers));
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
  networking.firewall.enable = false;
  nix.gc = {
    automatic = true;
    dates = "monthly";
    options = "-d";
  };

  # Program config
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

  # Normal users
  security.sudo.wheelNeedsPassword = false;
  services.openssh.gatewayPorts = "clientspecified";
  users.defaultUserShell = pkgs.zsh;
  users.users.user = {
    createHome = true;
    home = "/home/user";
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keyFiles = config.ssh-keys;
  };
  environment.systemPackages = with pkgs; [
    kitty.terminfo
    trash-cli
    tree
  ];

  imports = [
    ./web.nix
    (createServiceUser { name = "abottomod"; script = "${abottomod}/bin/start"; })
    (createServiceUser { name = "timeywimey"; script = "${timeywimey}/bin/start"; })
    (createServiceUser { name = "redox-world-map"; script = "${redox-world-map}/bin/start"; })
  ];

  # Services
  services.znc = {
    enable = true;
    confOptions = {
      userName = config.name;
      nick = config.name;
      passBlock = config.secret.zncPassBlock;
      networks = createZncServers {
        freenode = "chat.freenode.net";
        mozilla = "irc.mozilla.org";
      };
    };
  };
}
