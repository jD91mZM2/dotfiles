{ pkgs, inputs, ... }:

let
  shared = pkgs.callPackage <dotfiles/shared> {};
  nur-no-pkgs = import (<dotfiles/shared/nur-no-pkgs.nix>);

  # Packages
  abottomod = import ~/Coding/Python/abottomod;
  timeywimey = import ~/Coding/Python/timeywimey;
  redox-world-map = shared.builders.buildRustPackage {
    name        = "redox-world-map";
    src         = ~/Coding/Web/redox-world-map;
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
      size   = "s-1vcpu-1gb";
    };
    keys = let
      makeKey = keyFile: {
        permissions = "0644"; # TODO: make more restrictive later
        keyFile = /. + keyFile;
      };
    in {
      email = makeKey "${shared.consts.secrets}/email";
      bitwarden = makeKey "${shared.consts.secrets}/bitwarden";
      discord-abottomod = makeKey "${shared.consts.secrets}/discord/abottomod";
      discord-timeywimey = makeKey "${shared.consts.secrets}/discord/timeywimey";
      znc = makeKey "${shared.consts.secrets}/znc";
    };
  };

  imports = [
    # Shared base settings
    ../base.nix

    # Custom modules
    nur-no-pkgs.repos.jd91mzm2.modules.custom-services # <- recursion

    # Files
    ./bitwarden.nix
    ./email.nix
    ./web.nix
    ./znc.nix
  ];

  # Backup
  services.borgbackup.jobs.main = let
    repo = "/var/lib/backup";
  in {
    paths   = "/var/lib";
    exclude = [ repo ];
    inherit repo;
    encryption.mode = "none";
    startAt         = "daily";
    postHook        = ''
      echo "\$archiveName = $archiveName"
      ${pkgs.rclone}/bin/rclone sync -v "${repo}" "BackBlaze:jD91mZM2-backups/droplet-main"
      ${pkgs.rclone}/bin/rclone cleanup -v "BackBlaze:jD91mZM2-backups/droplet-main"
    '';
  };

  # Services
  custom.services = let
    normal = script: { inherit script; };
    withKeys = script: {
      inherit script;
      group = "keys";
    };
  in {
    abottomod       = withKeys "ABOTTOMOD_TOKEN=\"$(cat /run/keys/discord-abottomod)\" ${abottomod}/bin/abottomod";
    timeywimey      = withKeys "TIMEYWIMEY_TOKEN=\"$(cat /run/keys/discord-timeywimey)\" ${timeywimey}/bin/timeywimey";
    redox-world-map = normal "${redox-world-map}/bin/start";
  };
  services.syncthing = {
    enable = true;
    declarative = {
      overrideDevices = true;
      overrideFolders = false;
      devices         = builtins.removeAttrs shared.consts.syncthingDevices [ "droplet" ];
    };
  };
  services.do-agent.enable = true;
}
