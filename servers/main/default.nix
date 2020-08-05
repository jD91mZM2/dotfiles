{ pkgs, ... }:

let
  shared = pkgs.callPackage <dotfiles/shared> {};
  nur-no-pkgs = import (<dotfiles/shared/nur-no-pkgs.nix>);

  # Packages
  abottomod = shared.builders.buildPypiPackage {
    name = "abottomod";
    src  = ~/Coding/Python/abottomod;
  };
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
  };

  imports = [
    # Shared base settings
    ../base.nix

    # Custom modules
    nur-no-pkgs.repos.jd91mzm2.modules.custom-services

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
  custom.services = {
    # abottomod.script       = "${abottomod}/bin/start";
    timeywimey.script      = "TIMEYWIMEY_TOKEN=\"${shared.consts.secret.discordTokens.timeywimey}\" ${timeywimey}/bin/timeywimey";
    redox-world-map.script = "${redox-world-map}/bin/start";
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
