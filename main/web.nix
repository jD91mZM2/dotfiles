{ pkgs, ... }:

let
  config = import ./config.nix;
in {
  security.acme = {
    # for testing certificates, toggle comment below:
    # production = false;
    certs."krake.one" = {
      domain = "krake.one";
      email = config.email;
      webroot = "/var/www/challenges";
      postRun = ''
        systemctl restart nginx
      '';
      extraDomains = {
        "redox-os.club" = null;
      };
    };
  };
  services.tor = {
    enable = true;
    hiddenServices."rickroll".map = [
      { port = 80; toPort = 11694; }
    ];
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
      "krake.one:11694" = {
        useACMEHost = "krake.one";
        serverName = "krake.one";
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
        forceSSL = true;
        acmeRoot = "/var/www/challenges";
        locations."/" = {
          proxyPass = "http://localhost:22165";
        };
      };
    };
  };
}
