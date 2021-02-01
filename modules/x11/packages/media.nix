{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    mkchromecast
    sxiv
    vlc
  ];

  environment.shellAliases = {
    screencast = "mkchromecast -n \"Living Room TV\" --video --screencast";
  };
}
