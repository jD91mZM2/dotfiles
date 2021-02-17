{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # Viewing
    mpv
    sxiv
    vlc

    # Casting
    mkchromecast
  ];

  environment.shellAliases = {
    screencast = "mkchromecast -n \"Living Room TV\" --video --screencast";
  };
}
