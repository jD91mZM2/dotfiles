{ pkgs, self, shared, nur, ... }:

{
  # System overlays
  nixpkgs.overlays = self.overlays;

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
    powerline-rs = {
      enable = true;
      args = ["--cwd-max-depth" "3"];
    };
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

        # Free up the '#' symbol, for use in nix flakes
        unsetopt extendedglob
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
