{ pkgs, lib, ... }:

let
  aliases = {
    clear     = "clear; echo -ne \"\\e[3J\"";
    e         = "env -u TMPDIR emacsclient -n"; # nix-shell sets $TMPDIR which messes up emacsclient's search
    ls        = "ls -CF --color=auto";
    nix-shell = "nix-shell --command zsh";
    rsynca    = "rsync -avzhP --delete";
  };
  bashConfig = builtins.replaceStrings [ "  " ] [ "\t" ] ''
    eval "$(dircolors "${dircolors}")"
  '';

  dircolors = pkgs.fetchFromGitHub {
    owner  = "joshbenham";
    repo   = "linux-dotfiles";
    rev    = "67641154e7befa67527f73a6cbf64b36e15641ca";

    sha256 = "0hvnbc2wlx6j0p4k1znx72ma9rnvf55b9mcfays3pdn80qsx9s8q";
  } + "/dircolors/Dracula.dircolors";

  shared = pkgs.callPackage <dotfiles/shared> {};
  nur-no-pkgs = import (<dotfiles/shared/nur-no-pkgs.nix>);
in
{
  imports = [
    nur-no-pkgs.repos.jd91mzm2.hm-modules.programs
    nur-no-pkgs.repos.jd91mzm2.hm-modules.directlink

    ./bspwm.nix
    ./dunst.nix
    ./emacs.nix
    ./env.nix
    ./misc.nix
    ./packages.nix
    ./polybar.nix
  ];

  programs.home-manager = {
    enable = true;
    path   = https://github.com/rycee/home-manager/archive/master.tar.gz;
  };

  home.keyboard = null;

  #   ____ _     ___
  #  / ___| |   |_ _|
  # | |   | |    | |
  # | |___| |___ | |
  #  \____|_____|___|

  # Shells
  programs.bash = {
    enable = true;
    shellAliases = aliases;
    initExtra = bashConfig;
  };
  programs.zsh = {
    enable = true;
    shellAliases = aliases;
    initExtra = ''
      ${bashConfig}
    '';
  };

  # Misc
  programs.ssh = {
    enable      = true;
    matchBlocks = shared.consts.secret.sshHosts;
  };
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
  programs.gpg.enable = true;

  # Services
  services.gpg-agent = {
    enable             = true;
    enableSshSupport   = true;
    defaultCacheTtl    = 86400;
    defaultCacheTtlSsh = 86400;
    maxCacheTtl        = 86400;
    maxCacheTtlSsh     = 86400;
  };

  #   ____                 _     _           _
  #  / ___|_ __ __ _ _ __ | |__ (_) ___ __ _| |
  # | |  _| '__/ _` | '_ \| '_ \| |/ __/ _` | |
  # | |_| | | | (_| | |_) | | | | | (_| (_| | |
  #  \____|_|  \__,_| .__/|_| |_|_|\___\__,_|_|
  #                 |_|

  # Configs
  xresources = {
    extraConfig = builtins.readFile (pkgs.fetchFromGitHub {
      owner  = "dracula";
      repo   = "xresources";
      rev    = "ca0d05cf2b7e5c37104c6ad1a3f5378b72c705db";

      sha256 = "0ywkf2bzxkr45a0nmrmb2j3pp7igx6qvq6ar0kk7d5wigmkr9m5n";
    } + "/Xresources");
    properties = {
      # Everything
      "*.font" = "Hack:pixelsize=13:antialias=true:autohint=true";

      # XTerm stuff
      "XTerm.termName"          = "xterm-256color";
      "XTerm.vt100.faceName"    = "Hack:size =10";
      "XTerm*decTerminalID"     = "vt340";
      "XTerm*numColorRegisters" = 256;
    };
  };
  gtk = {
    enable = true;
    font = {
      name    = "Cantarell 11";
      package = pkgs.cantarell-fonts;
    };
    iconTheme = {
      name    = "Yaru";
      package = pkgs.yaru-dracula-theme;
    };
    theme = {
      name    = "Yaru-dark";
      package = pkgs.yaru-dracula-theme;
    };
  };
  qt = {
    enable = true;
    platformTheme = "gtk";
  };

  programs.firefox = {
    enable = true;
    privacy.extensions.enable = true;
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      # Other addons
      bitwarden
      stylus
      vimium
    ];
    profiles.main = {
      id = 0;
      name = "home-manager";
      privacy.enableSettings = true;
      settings = {
        # Fix issues with having a dark GTK theme
        "ui.use_standins_for_native_colors"   = true;
        "widget.content.allow-gtk-dark-theme" = false;
        "widget.chrome.allow-gtk-dark-theme"  = false;
        "widget.content.gtk-theme-override"   = "Adwaita:light";

        # Disable WebRTC because it's scary
        "media.peerconnection.enabled" = false;
      };
    };
  };

  # Services
  services.picom = {
    enable = true;
    # Apparently, using (intel + xrandr to configure multiple monitors + glx
    # backend) seems to cause all kinds of weird issues.
    backend = "xrender";

    fade        = true;
    fadeDelta   = 5;
    inactiveDim = "0.1";
    shadow      = true;
  };
}
