{ pkgs, shared, ... }:

let
  acmeRoot = "/var/lib/acme";

  whitelist = ''
    redox-os.org
    gitlab.redox-os.org
  '';
in
{
  # Configured using
  #
  # Simple NixOS Mailserver
  # https://gitlab.com/simple-nixos-mailserver/nixos-mailserver
  imports = [
    (builtins.fetchTarball {
      url    = "https://gitlab.com/simple-nixos-mailserver/nixos-mailserver/-/archive/4008d0cb53b9abd00d2dc568814566227ddd3825.tar.gz";
      sha256 = "1y9svi3nrg24ky1gqbpa3zhnhhin399h0cmnf81hb4yn75mlqiqx";
    })
  ];

  mailserver = {
    enable  = true;
    localDnsResolver = false;  # kresd is broken
    fqdn    = "mail.krake.one";
    domains = [ "krake.one" ];

    loginAccounts = {
      "me@krake.one" = {
        hashedPasswordFile = "/run/keys/email";

        # Really, really, old email address. Don't judge.
        aliases = [ "legolord208@krake.one" ];
        # For now, this is also my catch-all
        catchAll = [ "krake.one" ];
      };
    };

    # https://gitlab.com/simple-nixos-mailserver/nixos-mailserver/blob/v2.2.1/default.nix#L295-303
    certificateScheme = 1;
    certificateFile   = "${acmeRoot}/mail.krake.one/fullchain.pem";
    keyFile           = "${acmeRoot}/mail.krake.one/key.pem";

    # Enable IMAP/POP
    enableImap    = true;
    enablePop3    = true;
    enableImapSsl = true;
    enablePop3Ssl = true;

    virusScanning = false;
  };

  # Overwrite rspamd's whitelist settings
  services.rspamd.locals."whitelists.conf".text = ''
    whitelist {
      rules {
        whitelist_domains = {
          domains = "${pkgs.writeText "good-domains.map" whitelist}";
          score = -1.0;
        }
      }
    }
  '';
}
