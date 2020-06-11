{ pkgs, ... }:

let
  shared = pkgs.callPackage <dotfiles/shared> {};
  nur-no-pkgs = import ./nur-no-pkgs.nix;
in
{
  imports = [
    nur-no-pkgs.repos.jd91mzm2.modules.programs
  ];

  # System overlays
  nixpkgs.overlays = import <dotfiles/home/generic/overlays.nix>;

  # Must have packages
  environment.systemPackages = with pkgs; [
    # CLI applications
    direnv

    # Used by zsh
    autojump

    # CLI utils
    bc
    file
    git
    gnupg
    htop
    ncdu
    nethogs
    trash-cli
    tree
  ];

  # Services
  services.lorri.enable = true;

  # Programs
  programs = {
    powerline-rs.enable = true;
    bash = {
      enableCompletion = true;
      interactiveShellInit = ''
        source "${pkgs.autojump}/share/autojump/autojump.bash"
        eval "$("${pkgs.direnv}/bin/direnv" hook bash)"
      '';
    };
    zsh = {
      enable = true;
      autosuggestions = {
        enable = true;
        highlightStyle = "bg=#${(shared.theme.getColor 1).rgb},fg=#${(shared.theme.getColor 3).rgb}";
      };
      syntaxHighlighting.enable = true;
      vi.enable = true;
      interactiveShellInit = ''
        source "${pkgs.grml-zsh-config}/etc/zsh/zshrc"
        source "${pkgs.autojump}/share/autojump/autojump.zsh"
        eval "$("${pkgs.direnv}/bin/direnv" hook zsh)"

        set noextendglob
      '';
    };
  };

  # Default user
  users = {
    defaultUserShell = pkgs.zsh;
    users."${shared.consts.user}" = {
      # This is a hardcoded uid, used by any container. This means
      # files shared with containers are accessible.
      uid = 1000;

      isNormalUser = true;
      extraGroups = [ "wheel" ];
    };
  };
}
