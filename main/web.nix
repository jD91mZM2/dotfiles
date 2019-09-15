{ pkgs, ... }:

let
  config = import ./config.nix;
in {
  security.acme = {
    # for testing certificates, toggle comment below:
    # production = false;

    # Explicitly set this directory because it's used by email stuff
    directory = "/var/lib/acme";

    certs."krake.one" = {
      email = config.email;
      webroot = "/var/www/challenges";
      postRun = ''
        systemctl reload nginx
      '';
      extraDomains = {
        "redox-os.club" = null;
        "cloud.krake.one" = null;
      };
    };
    certs."mail.krake.one" = {
      email = config.email;
      webroot = "/var/www/challenges";

      # https://gitlab.com/simple-nixos-mailserver/nixos-mailserver/blob/v2.2.1/mail-server/nginx.nix#L39-41
      postRun = ''
        systemctl reload nginx
        systemctl reload postfix
        systemctl reload dovecot2
      '';
    };
  };
  services.tor = {
    enable = true;
    hiddenServices."rickroll".map = [
      { port = 80; toPort = 11694; }
    ];
  };
  services.nextcloud = {
    autoUpdateApps.enable = true;
    config = {
      adminpassFile = "/root/nextcloud-passwd";
      adminuser = config.name;
    };
    enable = true;
    hostName = "cloud.krake.one";
    https = true;
    nginx.enable = true;
  };
  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    virtualHosts = {
      "krake.one" = {
        useACMEHost = "krake.one";
        acmeRoot = "/var/www/challenges";
        forceSSL = true;

        default = true;
        locations."/" = {
          extraConfig = ''
            return 302 https://jd91mzm2.github.io/;
          '';
        };
      };
      "krake.one:1337" = {
        useACMEHost = "krake.one";
        serverName = "krake.one";
        onlySSL = true;

        listen = [{
          addr = "0.0.0.0";
          port = 1337;
          ssl = true;
        }];
        locations."/" = {
          root = "/var/www/public";
          extraConfig = ''
            autoindex on;
          '';
        };
      };
      "cloud.krake.one" = {
        useACMEHost = "krake.one";
        acmeRoot = "/var/www/challenges";
        forceSSL = true;
      };
      "mail.krake.one" = {
        useACMEHost = "mail.krake.one";
        acmeRoot = "/var/www/challenges";
        forceSSL = true;
      };
      "krake.one:11694" = {
        listen = [{
          # Domain only accessible from TOR
          addr = "127.0.0.1";
          port = 11694;
        }];
        locations."/" = {
          extraConfig = ''
            return 302 https://www.youtube.com/watch?v=dQw4w9WgXcQ;
          '';
        };
      };
      "redox-os.club" = {
        useACMEHost = "krake.one";
        acmeRoot = "/var/www/challenges";
        forceSSL = true;

        locations."/" = {
          proxyPass = "http://localhost:22165";
          extraConfig = ''
            error_page 502 /drop/502.html;
          '';
        };
        locations."/drop" = {
          proxyPass = "https://krake.one:1337/";
          extraConfig = ''
            internal;
          '';
        };
      };
    };
  };
}
