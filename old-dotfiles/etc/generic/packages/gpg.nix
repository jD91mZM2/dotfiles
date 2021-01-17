{ pkgs, lib, config, shared, ... }:

with lib;
let
  cfg = config.setup.packages.gpg;
in
{
  options.setup.packages.gpg = {
    enable = mkEnableOption "Enable GnuPG configuration";
  };

  config = mkIf cfg.enable {
    # Unlock GnuPG automagically
    security.pam.services.login.gnupg = {
      enable = true;
      noAutostart = true;
      storeOnly = true;
    };

    setup.home.modules = singleton {
      # Enable GPG agent
      programs.gpg.enable = true;
      services.gpg-agent = {
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

      # GPG settings
      programs.gpg.settings = {
        keyserver = "keys.openpgp.org";
      };

      # Allow manually restarting gpg-agent in case of failure
      systemd.user.services.gpg-agent.Unit.RefuseManualStart = lib.mkForce false;

      # Enter my ssh keys
      home.file.".pam-gnupg".text = ''
        B844DEE9FB33753FCFE216654430F649CA2FCC2B
        D827EEC5D8A0F0CA7DAC011270CC552C8953BBA1
        7AB7CA7DF6203E0F38B5B18F9BA3114B3DDBA750
        6157563FB7B5618A02F7D7490118A26A97F31CA9
      '';

      # Add gpg signing to git
      programs.git.signing = {
        key = shared.consts.gpgKeys.signing;
        signByDefault = true;
      };
    };
  };
}
