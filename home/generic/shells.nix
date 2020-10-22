{ pkgs, config, lib, shared, ... }:

let
  cfg = config.setup.shells;

  aliases = {
    cal        = "cal -m";
    clear      = "clear; echo -ne \"\\e[3J\"";
    e          = "env -u TMPDIR emacsclient -n"; # nix-shell sets $TMPDIR which messes up emacsclient's search
    ls         = "ls -CF --color=auto";
    nix-shell  = "nix-shell --command zsh";
    objdump    = "objdump -Mintel";
    rsynca     = "rsync -avzhP --delete";
    screencast = "mkchromecast -n \"Living Room TV\" --video --screencast";
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
      initExtra = ''
        ${bashConfig}
        unset -f trans # some alias by grml-zsh-config
      '';
    };

    # Remote shell
    programs.ssh = lib.mkIf cfg.enableGit {
      enable      = true;
      matchBlocks = {
        "main" = {
          user     = "user";
          hostname = "krake.one";
        };
      };
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
      aliases = {
        mr = "!f() { git fetch \${2-origin} merge-requests/\${1?}/head && git switch -d FETCH_HEAD; }; f";
      };
      extraConfig = {
        pull.rebase = true;
      };
    };

    # Every git needs some gpg...
    programs.gpg = lib.mkIf cfg.enableGit {
      enable = true;
      settings = {
        keyserver = "keys.openpgp.org";
      };
    };
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
