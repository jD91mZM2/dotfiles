{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    ffmpeg
    imagemagick
    gimp
    olive-editor
  ];
}
