{ config, pkgs, lib, ... }:

{
  services.gpg-agent = {
    enable             = true;
    enableSshSupport   = true;
    defaultCacheTtl    = 86400;
    defaultCacheTtlSsh = 86400;
    maxCacheTtl        = 86400;
    maxCacheTtlSsh     = 86400;
  };
  programs.gpg = {
    enable = true;
    settings = {
      # Best practices from https://www.void.gr/kargig/blog/2013/12/02/creating-a-new-gpg-key-with-subkeys/
      fixed-list-mode = true;
      keyid-format = "0xlong";
      personal-digest-preferences = "SHA512 SHA384 SHA256 SHA224";
      default-preference-list = "SHA512 SHA384 SHA256 SHA224 AES256 AES192 AES CAST5 BZIP2 ZLIB ZIP Uncompressed";
      use-agent = true;
      verify-options = "show-uid-validity";
      list-options = "show-uid-validity";
      cert-digest-algo = "SHA256";
      no-emit-version = true;
    };
  };
}
