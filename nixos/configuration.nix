# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  myPackages = {
    z = (pkgs.callPackage ./z.nix {});
  };
in
{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    # OpenVPN configs
    ./openvpn.nix
    # VPN killswitch
    ./killswitch.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Intel Microcode
  hardware.cpu.intel.updateMicrocode = true;

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Stockholm";

  # Networking
  networking.hostName = "compotar";
  networking.networkmanager.enable = true;
  networking.nameservers = ["1.1.1.1" "1.0.0.1"];

  # System packages
  environment.systemPackages = with pkgs; [
    # Look & Feel
    adapta-gtk-theme numix-icon-theme-circle xorg.xcursorthemes
    adapta-backgrounds
    libsForQt5.qtstyleplugins # uniform QT/GTK look
    # Shell
    neovim grml-zsh-config
    # Graphical - system
    dmenu j4-dmenu-desktop networkmanagerapplet compton
    # Graphical - applications
    firefox kdeApplications.konsole xfce.thunar
    # Utils
    git gitAndTools.hub fd ripgrep wget xclip htop socat gnupg file myPackages.z
  ];
  fonts.fonts = with pkgs; [
    cantarell-fonts
    font-awesome-ttf
    hack-font
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.bash = {
    enableCompletion = true;
    interactiveShellInit = ''
      source "${myPackages.z}/share/z.sh"
    '';
  };
  programs.slock.enable = true;
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    syntaxHighlighting.enable = true;
    interactiveShellInit = ''
      source "${pkgs.grml-zsh-config}/etc/zsh/zshrc"
      source "${myPackages.z}/share/z.sh"
    '';
  };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "eurosign:e";

  # Enable touchpad support.
  # services.xserver.libinput.enable = true;

  # X11 Setup
  services.xserver.displayManager.lightdm = {
    enable = true;
    background = "${pkgs.adapta-backgrounds}/share/backgrounds/adapta/tealized.jpg";
    greeters.gtk = {
      enable = true;
      iconTheme = {
        name = "Numix-Circle";
        package = pkgs.numix-icon-theme-circle;
      };
      theme = {
        name = "Adapta";
        package = pkgs.adapta-gtk-theme;
      };
    };
  };
  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
  };
  services.xserver.desktopManager.xfce.enable = true; # This is actually just for a working xfce4 panel

  services.gnome3.gnome-keyring.enable = true;
  security.pam.services.lightdm.enableGnomeKeyring = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraGroups.user = {};
  users.extraUsers.user = {
    isNormalUser = true;
    group = "user";
    extraGroups = ["wheel" "audio"];
    shell = pkgs.zsh;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?
}
