# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    # Monitor configs
    #./monitors.nix
    # System packages
    ./packages.nix
    # OpenVPN configs
    ./openvpn.nix
    # VPN killswitch
    ./killswitch.nix
  ];

  # systemd-boot
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Intel microcode
  hardware.cpu.intel.updateMicrocode = true;

  # TTY settings
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Time
  time.timeZone = "Europe/Stockholm";

  # Networking
  networking.hostName = "compotar";
  networking.networkmanager.enable = true;
  networking.nameservers = ["1.1.1.1" "1.0.0.1"];
  ## Manually overwrite /etc/resolv.conf because openresolv tries to add my ISP's DNS
  environment.etc."resolv.conf".text = ''
    ${builtins.concatStringsSep
        "\n"
        (map (ip: "nameserver " + ip) config.networking.nameservers)}
  '';

  # Sound
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # System environment stuff
  environment.variables.DEJA_DUP_MONITOR = "${pkgs.deja-dup}/libexec/deja-dup/deja-dup-monitor";

  ## Required by xfce4-panel
  environment.pathsToLink = [ "/share/xfce4" ];
  ## https://github.com/NixOS/nixpkgs/issues/33231
  environment.variables.GDK_PIXBUF_MODULE_FILE = "${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache";

  # Program configuration
  programs.bash = {
    enableCompletion = true;
    interactiveShellInit = ''
      source "${pkgs.autojump}/share/autojump/autojump.bash"
    '';
  };
  programs.slock.enable = true;
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    syntaxHighlighting.enable = true;
    promptInit = "";
    interactiveShellInit = ''
      source "${pkgs.grml-zsh-config}/etc/zsh/zshrc"
      source "${pkgs.autojump}/share/autojump/autojump.zsh"
    '';
  };
  virtualisation.docker = {
    enable = true;
    autoPrune.enable = true;
  };
  virtualisation.libvirtd.enable = true;

  # Graphics! X11! Much wow!
  services.xserver = {
    enable = true;
    layout = "us";
    xkbOptions = "compose:ralt";

    # Touchpad:
    # libinput.enable = true;

    displayManager.lightdm = {
      enable = true;
      background = "${pkgs.adapta-backgrounds}/share/backgrounds/adapta/tealized.jpg";
      greeters.gtk = {
        enable = true;
        theme = {
          name = "Adapta";
          package = pkgs.adapta-gtk-theme;
        };
      };
    };
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
  };

  # User settings
  users.extraUsers.user = {
    isNormalUser = true;
    extraGroups = ["wheel" "docker" "libvirtd"];
    shell = pkgs.zsh;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?
}
