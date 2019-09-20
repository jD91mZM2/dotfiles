{ pkgs, ... }:

let
  shared = pkgs.callPackage <dotfiles/shared> {};

  #  ____            _
  # |  _ \ __ _  ___| | ____ _  __ _  ___  ___
  # | |_) / _` |/ __| |/ / _` |/ _` |/ _ \/ __|
  # |  __/ (_| | (__|   < (_| | (_| |  __/\__ \
  # |_|   \__,_|\___|_|\_\__,_|\__, |\___||___/
  #                            |___/

  buildPypiPackage = pkgs.callPackage ./builders/pypi.nix {};
  buildRustPackage = pkgs.callPackage ./builders/rust.nix {};

  abottomod = buildPypiPackage {
    name = "abottomod";
    src = ~/Coding/Python/abottomod;
  };
  timeywimey = buildPypiPackage {
    name = "timeywimey";
    src = ~/Coding/Python/timeywimey;
  };
  redox-world-map = buildRustPackage {
    name = "redox-world-map";
    src = ~/Coding/Web/redox-world-map;
    buildInputs = with pkgs; [ pkgconfig openssl sqlite ];
    wrapperHook = ''
      ln -sf $out/src/Rocket.toml .
    '';
  };

  #  _   _      _
  # | | | | ___| |_ __   ___ _ __ ___
  # | |_| |/ _ \ | '_ \ / _ \ '__/ __|
  # |  _  |  __/ | |_) |  __/ |  \__ \
  # |_| |_|\___|_| .__/ \___|_|  |___/
  #              |_|

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
  #  __  __      _            _       _
  # |  \/  | ___| |_ __ _  __| | __ _| |_ __ _
  # | |\/| |/ _ \ __/ _` |/ _` |/ _` | __/ _` |
  # | |  | |  __/ || (_| | (_| | (_| | || (_| |
  # |_|  |_|\___|\__\__,_|\__,_|\__,_|\__\__,_|

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

  #  _   _
  # | | | |___  ___ _ __ ___
  # | | | / __|/ _ \ '__/ __|
  # | |_| \__ \  __/ |  \__ \
  #  \___/|___/\___|_|  |___/

  security.sudo.wheelNeedsPassword = false;
  services.openssh.gatewayPorts = "clientspecified";
  users.defaultUserShell = pkgs.zsh;
  users.users.user = {
    createHome = true;
    home = "/home/user";
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keyFiles = shared.consts.sshKeys;
  };
  environment.systemPackages = with pkgs; [
    file
    htop
    kitty.terminfo
    sqlite
    trash-cli
    tree
  ];

  disabledModules = [ "services/networking/syncthing.nix" ];
  imports = [
    # Files
    ./email.nix
    ./web.nix

    # Generated services
    (createServiceUser { name = "abottomod"; script = "${abottomod}/bin/start"; })
    (createServiceUser { name = "timeywimey"; script = "${timeywimey}/bin/start"; })
    (createServiceUser { name = "redox-world-map"; script = "${redox-world-map}/bin/start"; })

    # Unstable modules
    <nixos-unstable/nixos/modules/services/networking/syncthing.nix>
  ];

  #  ____                  _
  # / ___|  ___ _ ____   _(_) ___ ___  ___
  # \___ \ / _ \ '__\ \ / / |/ __/ _ \/ __|
  #  ___) |  __/ |   \ V /| | (_|  __/\__ \
  # |____/ \___|_|    \_/ |_|\___\___||___/

  services.syncthing = {
    enable = true;
    declarative = {
      overrideDevices = true;
      devices = {
        computer = {
          id = "ILTIRMY-JT4SGSQ-AWETWCV-SLQYHE6-CY2YGAS-P3EGWY6-LSP7H4Z-F7ZQIAN";
          introducer = true;
        };
        phone = {
          id = "O7H6BPC-PKQPTT4-T4SEA7K-VI7HJ4K-J7ZJO5K-NWLNAK5-RBVCSBU-EXDHSA3";
        };
      };
      overrideFolders = false;
    };
    relay = {
      enable = true;
      providedBy = "krake.one on DigitalOcean";
    };
  };
  services.znc = {
    enable = true;
    confOptions = {
      userName = shared.consts.name;
      nick = shared.consts.name;
      passBlock = shared.consts.secret.zncPassBlock;
      networks = createZncServers {
        freenode = "chat.freenode.net";
        mozilla = "irc.mozilla.org";
      };
    };
  };
}
