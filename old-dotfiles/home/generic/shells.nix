{ pkgs, config, lib, shared, ... }:

let
  cfg = config.setup.shells;
in
{
  options.setup.shells = {
    enableZsh = lib.mkEnableOption "zsh";
    enableBash = lib.mkEnableOption "bash";
    enableGit = lib.mkEnableOption "git";

    personal = lib.mkEnableOption "personal stuffs";
  };

  config = lib.mkMerge [
    {
      # Every shell needs some git...
      programs.git = lib.mkIf cfg.enableGit {
        enable = true;
        lfs.enable = true;

        aliases = {
          mr = "!f() { git fetch \${2-origin} merge-requests/\${1?}/head && git switch -d FETCH_HEAD; }; f";
          pr = "!f() { git fetch \${2-origin} pull/\${1?}/head && git switch -d FETCH_HEAD; }; f";
        };
        extraConfig = {
          pull.rebase = true;
        };
      };
    }

    (lib.mkIf cfg.personal {
      # SSH config
      programs.ssh = {
        enable = true;
        matchBlocks = {
          "main" = {
            user = "user";
            hostname = "krake.one";
          };
        };
      };

      # Git settings
      programs.git = {
        userName = shared.consts.name;
        userEmail = shared.consts.email;
      };
    })
  ];
}
