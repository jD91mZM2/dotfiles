{ pkgs, ... }:

let
  aliases = {
    clear     = "clear; echo -ne \"\\e[3J\"";
    e         = "env -u TMPDIR emacsclient -n"; # nix-shell sets $TMPDIR which messes up emacsclient's search
    ls        = "ls -CF --color=auto";
    nix-shell = "nix-shell --command zsh";
    rsynca    = "rsync -avzhP --delete";
  };

  dircolors = pkgs.fetchFromGitHub {
    owner  = "joshbenham";
    repo   = "linux-dotfiles";
    rev    = "67641154e7befa67527f73a6cbf64b36e15641ca";

    sha256 = "0hvnbc2wlx6j0p4k1znx72ma9rnvf55b9mcfays3pdn80qsx9s8q";
  } + "/dircolors/Dracula.dircolors";

  bashConfig = builtins.replaceStrings [ "  " ] [ "\t" ] ''
    eval "$(dircolors "${dircolors}")"
  '';

  shared = pkgs.callPackage <dotfiles/shared> {};
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
