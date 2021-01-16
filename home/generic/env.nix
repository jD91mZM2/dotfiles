{ pkgs, config, lib, shared, ... }:

let
  cfg = config.setup.env;
in
{
  options.setup.env = {
    enable = lib.mkEnableOption "env";
  };

  config = lib.mkIf cfg.enable {
    home.sessionVariables = {
      # Override PATH
      PATH = "$PATH:$HOME/.cargo/bin";

      # Standard configs
      EDITOR = "e-wait";
      VISUAL = "e-wait";

      # Program configs
      RUST_BACKTRACE = "1";
      GOPATH = "$HOME/Coding/Go";

      # https://github.com/electron/electron/issues/8455
      ELECTRON_FORCE_WINDOW_MENU_BAR = "1";

      # https://bbs.archlinux.org/viewtopic.php?id = 159016
      _JAVA_AWT_WM_NONREPARENTING = "1";
    };
  };
}
