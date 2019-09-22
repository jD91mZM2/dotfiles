{ pkgs, lib, ... }:

let
  shared = pkgs.callPackage <dotfiles/shared> {};

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
  ];

  # Services
  custom.services = {
    timeywimey.script = "${timeywimey}/bin/start";
    redox-world-map.script = "${redox-world-map}/bin/start";
  };
  services.syncthing = {
    declarative.devices = shared.utils.without [ "droplet" ] shared.consts.syncthingDevices;
    relay = {
      enable = true;
      providedBy = "krake.one on DigitalOcean";
    };
  };
  services.znc = {
    enable = true;
    useLegacyConfig = false;

    # Modules such as SASL are handled using a separate config, and
    # should be mutable anyway :)
    mutable = false;

    config = {
      LoadModule = [ "webadmin" "adminlog" ];
      Listener.l = {
        Port = 5000;
        IPv4 = true;
        IPv6 = true;
        SSL = true;
      };
      User."${shared.consts.name}" = {
        Admin = true;
        Nick = shared.consts.name;
        AltNick = shared.consts.name + "_";
        LoadModule = [ "chansaver" "controlpanel" ];
        Network = let
          createZncServers = servers:
            lib.mapAttrs
              (_name: cfg: {
                Server = "${cfg.ip} +6697";
                LoadModule = [ "simple_away" "sasl" "keepnick" ];
                Chan = lib.listToAttrs (
                  map
                    (name: lib.nameValuePair name {})
                    cfg.chan
                );
              })
              servers;
        in
          createZncServers {
            freenode = {
              ip = "chat.freenode.net";
              chan = [ "#nixos" "#nixos-chat" "#nix-community" ];
            };
            mozilla = {
              ip = "irc.mozilla.org";
              chan = [ "#rust" ];
            };
          };
        Pass.password = shared.consts.secret.zncPassBlock;
      };
    };
  };
}
