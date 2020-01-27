{ pkgs, ... }:

let
  shared   = pkgs.callPackage <dotfiles/shared> {};
  acmeRoot = "/var/lib/acme";
in
{
  # Configured using
  #
  # Simple NixOS Mailserver
  # https://gitlab.com/simple-nixos-mailserver/nixos-mailserver
  imports = [
    (builtins.fetchTarball {
      url    = "https://gitlab.com/simple-nixos-mailserver/nixos-mailserver/-/archive/v2.3.0/nixos-mailserver-v2.3.0.tar.gz";
      sha256 = "0lpz08qviccvpfws2nm83n7m2r8add2wvfg9bljx9yxx8107r919";
    })
  ];

  mailserver = {
    enable  = true;
    localDnsResolver = false;  # kresd is broken
    fqdn    = "mail.krake.one";
    domains = [ "krake.one" ];

    loginAccounts = {
      "me@krake.one" = {
        hashedPassword = shared.consts.secret.emailPass;

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
}
