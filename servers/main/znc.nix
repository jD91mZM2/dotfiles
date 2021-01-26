{ pkgs, lib, config, ... }:

{
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
      User."${config.globals.name}" = {
        Admin = true;
        Nick = config.globals.name;
        AltNick = config.globals.name + "_";
        LoadModule = [ "chansaver" "controlpanel" ];
        Network =
          let
            createZncServers = servers:
              lib.mapAttrs
                (_name: cfg: {
                  Server = "${cfg.ip} +6697";
                  LoadModule = [ "simple_away" "sasl" "keepnick" ];
                  Chan = lib.listToAttrs (
                    map
                      (name: lib.nameValuePair name { })
                      cfg.chan
                  );
                })
                servers;
          in
          createZncServers {
            freenode = {
              ip = "chat.freenode.net";
              chan = [ "#nixos" "#nixos-chat" "#nix-community" "#haskell.nix" ];
            };
            mozilla = {
              ip = "irc.mozilla.org";
              chan = [ "#rust" ];
            };
          };

        # TODO: Find some way to use `passwordFile`-like thing with ZNC
        # extraConfigFile = "/var/lib/secrets/znc";
        extraConfig = builtins.readFile "${config.globals.syncthingHome}/secrets/znc";
      };
    };
  };
}
