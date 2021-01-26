{ pkgs, inputs, system, config, ... }:
let
  # Packages
  redox-world-map = inputs.redox-world-map.defaultPackage."${system}".override {
    # TODO: Make these into runtime secrets
    clientId = "2247a648fd9f3f852ef66a5d876fed20421d0a974f67bc3e9bf0926048d831c1";
    clientSecret = builtins.readFile /home/user/Sync/secrets/redox-world-map;
  };

  # Helpers
in
{
  imports = [
    ../base
    ../../modules/user/extra/syncthing.nix

    # Files
    ./bitwarden.nix
    ./discord.nix
    ./email.nix
    ./hardware.nix
    ./syncthing.nix
    ./web.nix
    ./znc.nix
  ];

  deployment.targetHost = "root@krake.one";

  # Backup
  services.borgbackup.jobs.main =
    let
      repo = "/var/lib/backup";
    in
    {
      paths = [
        "/var/lib"
        "/home/user"
      ];
      exclude = [ repo ];
      inherit repo;
      encryption.mode = "none";
      startAt = "daily";
      prune.keep = {
        daily = 7;
        weekly = 4;
        monthly = 3;
      };
      postHook = ''
        echo "\$archiveName = $archiveName"
        "${pkgs.rclone}/bin/rclone" sync -v "${repo}" "BackBlaze:jD91mZM2-backups/vultr-main"
        "${pkgs.rclone}/bin/rclone" cleanup -v "BackBlaze:jD91mZM2-backups/vultr-main"
      '';
    };

  # Services
  custom.services =
    let
      normal = script: { inherit script; };
    in
    {
      redox-world-map = {
        script = "${redox-world-map}/bin/redox-world-map";
      };
    };
}
