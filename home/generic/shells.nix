{ pkgs, config, lib, ... }:

let
  shared = pkgs.callPackage <dotfiles/shared> {};
  cfg = config.setup.shells;

  aliases = {
    clear     = "clear; echo -ne \"\\e[3J\"";
    e         = "env -u TMPDIR emacsclient -n"; # nix-shell sets $TMPDIR which messes up emacsclient's search
    ls        = "ls -CF --color=auto";
    nix-shell = "nix-shell --command zsh";
    rsynca    = "rsync -avzhP --delete";
  };

  bashConfig = builtins.replaceStrings [ "  " ] [ "\t" ] ''
    # No dircolors, the defaults look good to me... for now

    if [ "$TERM" = "linux" ]; then
      ${toString
        (map
          (color: ''
            echo -ne "\e]P${color.hex}${color.rgb}"
          '')
          shared.theme.colors)}
    fi
  '';
in {
  options.setup.shells = {
    enableZsh = lib.mkEnableOption "zsh";
    enableBash = lib.mkEnableOption "bash";
    enableGit = lib.mkEnableOption "git";
  };

  config = {
    # Shells
    programs.bash = lib.mkIf cfg.enableBash {
      enable = true;
      shellAliases = aliases;
      initExtra = bashConfig;
    };
    programs.zsh = lib.mkIf cfg.enableZsh {
      enable = true;
      shellAliases = aliases;
      initExtra = bashConfig;
    };

    # Remote shell
    programs.ssh = lib.mkIf cfg.enableGit {
      enable      = true;
      matchBlocks = shared.consts.secret.sshHosts;
    };

    # Every shell needs some git...
    programs.git = lib.mkIf cfg.enableGit {
      enable     = true;
      lfs.enable = true;
      userName   = shared.consts.name;
      userEmail  = shared.consts.email;

      signing = {
        key = shared.consts.gpgKeys.signing;
        signByDefault = true;
      };
      extraConfig = {
        pull.rebase = true;
      } // shared.consts.secret.gitConfig;
    };

    # Every git needs some gpg...
    programs.gpg.enable = lib.mkIf cfg.enableGit true;
    services.gpg-agent = lib.mkIf cfg.enableGit {
      enable             = true;
      enableSshSupport   = true;
      defaultCacheTtl    = 86400;
      defaultCacheTtlSsh = 86400;
      maxCacheTtl        = 86400;
      maxCacheTtlSsh     = 86400;
    };
  };
}
