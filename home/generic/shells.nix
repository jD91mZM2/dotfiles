{ pkgs, config, lib, shared, ... }:

let
  cfg = config.setup.shells;
in
{
  options.setup.shells = {
    enableZsh = lib.mkEnableOption "zsh";
    enableBash = lib.mkEnableOption "bash";
    enableGit = lib.mkEnableOption "git";
    enableGnuPG = lib.mkEnableOption "GnuPG";

    personal = lib.mkEnableOption "personal stuffs";
  };

  config = lib.mkMerge [
    {
      # Every shell needs some git...
      programs.git = lib.mkIf cfg.enableGit {
        enable = true;
        lfs.enable = true;

        aliases = {
          mr = "!f() { git fetch \${2-origin} merge-requests/\${1?}/head && git switch -d FETCH_HEAD; }; f";
          pr = "!f() { git fetch \${2-origin} pull/\${1?}/head && git switch -d FETCH_HEAD; }; f";
        };
        extraConfig = {
          pull.rebase = true;
        };
      };

      # Every git needs some gpg...
      programs.gpg.enable = true;
      services.gpg-agent = lib.mkIf cfg.enableGnuPG {
        enable = true;
        enableSshSupport = true;
        defaultCacheTtl = 86400;
        defaultCacheTtlSsh = 86400;
        maxCacheTtl = 86400;
        maxCacheTtlSsh = 86400;
        extraConfig = ''
          allow-preset-passphrase
        '';
      };

      # Allow manually restarting gpg-agent in case of failure
      systemd.user.services.gpg-agent.Unit.RefuseManualStart = lib.mkForce false;
    }

    (lib.mkIf cfg.personal {
      # SSH config
      programs.ssh = {
        enable = true;
        matchBlocks = {
          "main" = {
            user = "user";
            hostname = "krake.one";
          };
        };
      };

      # Git settings
      programs.git = {
        userName = shared.consts.name;
        userEmail = shared.consts.email;

        signing = {
          key = shared.consts.gpgKeys.signing;
          signByDefault = true;
        };
      };

      # GPG settings
      programs.gpg.settings = {
        keyserver = "keys.openpgp.org";
      };
      home.file.".pam-gnupg".text = ''
        B844DEE9FB33753FCFE216654430F649CA2FCC2B
        D827EEC5D8A0F0CA7DAC011270CC552C8953BBA1
        7AB7CA7DF6203E0F38B5B18F9BA3114B3DDBA750
        6157563FB7B5618A02F7D7490118A26A97F31CA9
      '';
    })
  ];
}
