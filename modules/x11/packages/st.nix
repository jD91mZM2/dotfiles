{ pkgs, self, system, ... }:
let
  st = self.packages."${system}".st;
  neovim = self.packages."${system}".neovim;
in
{
  # Since st is so vital, it is already installed in `x11/base.nix`. All this
  # does is activate autostart and add desktop entries.

  desktopItems = {
    st = {
      name = "Simple Terminal";
      exec = "${st}/bin/st";
      autostart = true;
    };
    neovim = {
      name = "NeoVim";
      exec = "${st}/bin/st -n neovim ${neovim}/bin/e";
      autostart = true;
    };
    weechat = {
      name = "WeeChat";
      exec = "${st}/bin/st -n weechat ${pkgs.weechat}/bin/weechat";
      autostart = true;
    };
  };
}
