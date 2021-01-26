{ pkgs, ... }:
{
  deployment.secrets.email = {
    keySource = "email";
  };

  # Configured using
  #
  # Simple NixOS Mailserver
  # https://gitlab.com/simple-nixos-mailserver/nixos-mailserver
  mailserver = {
    enable = true;
    fqdn = "mail.krake.one";
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
    certificateFile = "/var/lib/acme/mail.krake.one/fullchain.pem";
    keyFile = "/var/lib/acme/mail.krake.one/key.pem";

    # Enable IMAP/POP
    enableImap = true;
    enablePop3 = true;
    enableImapSsl = true;
    enablePop3Ssl = true;

    virusScanning = false;
  };
}
