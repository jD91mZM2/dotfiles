{ pkgs, ... }:

let
  shared = pkgs.callPackage <dotfiles/shared> {};
  generators = import <dotfiles/shared/generators.nix>;

  # Packages
  timeywimey = shared.builders.buildPypiPackage {
    name = "timeywimey";
    src = ~/Coding/Python/timeywimey;
  };
  redox-world-map = shared.builders.buildRustPackage {
    name = "redox-world-map";
    src = ~/Coding/Web/redox-world-map;
    buildInputs = with pkgs; [ pkgconfig openssl sqlite ];
    wrapperHook = ''
      ln -sf $out/src/Rocket.toml .
    '';
  };

  # Helpers
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
  # Metadata
  deployment = {
    targetEnv = "digitalOcean";
    digitalOcean = {
      region = "ams3";
      size = "s-1vcpu-1gb";
    };
  };

  disabledModules = [ "services/networking/syncthing.nix" ];
  imports = [
    # Shared base settings
    ../base.nix
    ../syncthing.nix

    # Files
    ./email.nix
    ./web.nix

    # Generated services
    (generators.serviceUser { name = "timeywimey"; script = "${timeywimey}/bin/start"; })
    (generators.serviceUser { name = "redox-world-map"; script = "${redox-world-map}/bin/start"; })
  ];

  # Services
  services.syncthing = {
    declarative.devices.rpi.id = "AJEYZR5-OVJCWLD-SF37XSB-M2YSGMA-M7W33PW-S7JRWZM-ZLD6F33-KPSI3QD";
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
