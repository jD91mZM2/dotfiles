{ pkgs, lib, ... }:

let
  shared = pkgs.callPackage <dotfiles/shared> {};
in
{
  services.bitwarden_rs = {
    enable = true;
    config = {
      signupsAllowed = false;
      rocketPort     = 27057;
      logFile        = "/var/lib/bitwarden_rs/log";
    };
    # environmentFile = "/run/keys/bitwarden";
  };

  # See https://github.com/dani-garcia/bitwarden_rs/wiki/Fail2Ban-Setup
  environment.etc."fail2ban/filter.d/bitwarden-admin.conf".text = ''
    [INCLUDES]
    before = common.conf

    [Definition]
    failregex = ^.*Invalid admin token\. IP: <ADDR>.*$
    ignoreregex =
  '';
  services.fail2ban = {
    enable = true;
    jails = {
      bitwarden = ''
        # If you make 5 failed attempts
        logpath  = /var/lib/bitwarden_rs/log
        filter   = bitwarden
        maxretry = 5

        # Ban the IP for an entire day
        bantime  = 86400
        findtime = 86400

        # ... using iptables
        port   = 80,443,8081
        action = iptables-allports[name=bitwarden]
      '';
      bitwarden-admin = ''
        # If you make 3 failed attempts
        logpath  = /var/lib/bitwarden_rs/log
        filter   = bitwarden-admin
        maxretry = 3

        # Ban the IP for an entire day
        bantime  = 86400
        findtime = 86400

        # ... using iptables
        port   = 80,443
        action = iptables-allports[name=bitwarden-admin]
      '';
    };
  };
}
