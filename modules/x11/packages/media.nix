{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    mkchromecast
    mpv
    sxiv
    vlc
  ];

  environment.shellAliases = {
    screencast = "mkchromecast -n \"Living Room TV\" --video --screencast";
  };
}
