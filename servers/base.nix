{ pkgs, ... }:
let
  shared = pkgs.callPackage <dotfiles/shared> {};
in
{
  imports = [
    # Custom modules
    <dotfiles/shared/modules/services.nix>
  ];

  # Language settings
  i18n.consoleKeyMap = "dvorak";
  services.xserver.layout = "dvorak";

  # System config
  networking.firewall.enable = false;
  nix.gc = {
    automatic = true;
    dates     = "monthly";
    options   = "-d";
  };

  # OpenSSH settings
  services.openssh = {
    enable                 = true;
    forwardX11             = true;
    gatewayPorts           = "clientspecified";
    passwordAuthentication = false;
  };

  # User settings
  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };
  users.defaultUserShell = pkgs.zsh;

  # Default user
  users.users.user = {
    createHome = true;
    home = "/home/user";
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keyFiles = shared.consts.sshKeys;
  };

  # Default packages
  environment.systemPackages = with pkgs; [
    file
    htop
    kitty.terminfo
    rclone
    sqlite
    trash-cli
    tree
  ];

  # Program config
  programs.mosh.enable = true;
  programs.zsh = {
    enable = true;
    autosuggestions.enable = true;
    promptInit = ''
      powerline() {
        PS1="$(${pkgs.powerline-rs}/bin/powerline-rs --shell zsh "$?")"
      }
      precmd_functions+=(powerline)
    '';
    interactiveShellInit = ''
      . ${pkgs.grml-zsh-config}/etc/zsh/zshrc
    '';
    syntaxHighlighting.enable = true;
  };
}
