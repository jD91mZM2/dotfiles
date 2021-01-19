{ pkgs, self, system, ... }:
let
  st = self.packages."${system}".st;
  neovim = self.packages."${system}".neovim;
in
{
  # Since st is so vital, it is already installed in `x11/base.nix`. All this
  # does is activate autostart and add desktop entries.

  # Add desktop entries
  environment.systemPackages = [
    (pkgs.makeDesktopItem {
      name = "st";
      desktopName = "Simple Terminal";
      exec = "${st}/bin/st";
    })
    (pkgs.makeDesktopItem {
      name = "neovim";
      desktopName = "NeoVim";
      exec = "${st}/bin/st -n neovim ${neovim}/bin/e";
    })
    (pkgs.makeDesktopItem {
      name = "weechat";
      desktopName = "WeeChat";
      exec = "${st}/bin/st -n weechat ${pkgs.weechat}/bin/weechat";
    })
  ];

  # Autostart
  home.xsession.initExtra = ''
    # St
    ${st}/bin/st &

    # Neovim
    ${st}/bin/st -n neovim ${neovim}/bin/e &

    # WeeChat
    ${st}/bin/st -n weechat ${pkgs.weechat}/bin/weechat &
  '';
}
