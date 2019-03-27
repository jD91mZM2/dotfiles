{ pkgs, ... }:

let
  config = import ./config.nix;
in {
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 80 443 1337 ];
  };
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
        onlySSL = true; # without this, ssl_certificate field is not generated
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
