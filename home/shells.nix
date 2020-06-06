{ pkgs, ... }:

let
  shared = pkgs.callPackage <dotfiles/shared> {};

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
      ${shared.theme.mapStr (color: ''
          echo -ne "\e]P${color.hex}${color.rgb}"
        '')}
    fi
  '';
in
{
  # Shells
  programs.bash = {
    enable = true;
    shellAliases = aliases;
    initExtra = bashConfig;
  };
  programs.zsh = {
    enable = true;
    shellAliases = aliases;
    initExtra = bashConfig;
  };

  # Remote shell
  programs.ssh = {
    enable      = true;
    matchBlocks = shared.consts.secret.sshHosts;
  };

  # Every shell needs some git...
  programs.git = {
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
  programs.gpg.enable = true;
  services.gpg-agent = {
    enable             = true;
    enableSshSupport   = true;
    defaultCacheTtl    = 86400;
    defaultCacheTtlSsh = 86400;
    maxCacheTtl        = 86400;
    maxCacheTtlSsh     = 86400;
  };
}
