{ config, pkgs, ... }:

let
  # Packages
  abottomod = (pkgs.callPackage ./pypi.nix {}) {
    name = "abottomod";
    src = ~/Coding/Python/abottomod;
  };
  timeywimey = (pkgs.callPackage ./pypi.nix {}) {
    name = "timeywimey";
    src = ~/Coding/Python/timeywimey;
  };
  mcbotface = (pkgs.callPackage ./rust.nix {}) {
    name = "mcbotface";
    src = ~/Coding/Rust/mcbotface;
    buildInputs = with pkgs; [ pkgconfig openssl sqlite ];
  };
  redox-world-map = (pkgs.callPackage ./rust.nix {}) {
    name = "redox-world-map";
    src = ~/Coding/Web/redox-world-map;
    buildInputs = with pkgs; [ pkgconfig openssl sqlite ];
    wrapperHook = ''
      ln -s $out/src/Rocket.toml . || true
    '';
  };

  # Misc
  createServiceUser = { name, script }: {
    users.users."${name}" = {
      createHome = true;
      home = "/var/lib/${name}";
    };
    systemd.services."${name}" = {
      inherit script;
      serviceConfig = {
        User = name;
        WorkingDirectory = "/var/lib/${name}";
        Restart = "always";
        RestartSec = "10s";
      };
      wantedBy = [ "multi-user.target" ];
    };
  };
  ssh-keys = [ ~/.ssh/id_ed25519.pub ];
  email = "me@krake.one";
in {
  # Deployment metadata
  deployment = {
    targetEnv = "digitalOcean";
    digitalOcean = {
      region = "ams3";
      size = "s-1vcpu-1gb";
    };
  };

  # System config
  programs.mosh.enable = true;
  programs.zsh = {
    enable = true;
    autosuggestions.enable = true;
    promptInit = ''
      powerline() {
        PS1="$(${pkgs.powerline-rs}/bin/powerline-rs --shell zsh "$?")"
      }
      precmd_functions+=(powerline)
    '';
    interactiveShellInit = ''
      . ${pkgs.grml-zsh-config}/etc/zsh/zshrc
    '';
    syntaxHighlighting.enable = true;
  };

  # Normal users
  security.sudo.wheelNeedsPassword = false;
  users.defaultUserShell = pkgs.zsh;
  users.users.user = {
    createHome = true;
    home = "/home/user";
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keyFiles = ssh-keys;
  };
  environment.systemPackages = with pkgs; [
      trash-cli
  ];

  imports = [
    (createServiceUser { name = "abottomod"; script = "${abottomod}/bin/start"; })
    (createServiceUser { name = "timeywimey"; script = "${timeywimey}/bin/start"; })
    (createServiceUser { name = "mcbotface"; script = "${mcbotface}/bin/start"; })
    (createServiceUser { name = "redox-world-map"; script = "${redox-world-map}/bin/start"; })
  ];

  # Webserver
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 80 443 1337 ];
  };
  security.acme = {
    # for testing certificates, toggle comment below:
    # production = false;
    certs."krake.one" = {
      inherit email;
      domain = "krake.one";
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
