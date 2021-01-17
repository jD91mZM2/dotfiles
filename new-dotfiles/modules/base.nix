{ pkgs, ... }:

{
  # Set default locale
  i18n.defaultLocale = "en_GB.UTF-8";

  # Set timezone
  time.timeZone = "Europe/Stockholm";

  # Nix flakes
  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  # Base packages
  environment.systemPackages = with pkgs; [
    git
    tmux
  ];
}
