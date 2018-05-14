{
  PATH = "$PATH:$HOME/.local/bin:$HOME/.cargo/bin:$HOME/Coding/Go/bin:$HOME/redox/deb/usr/bin";

  EDITOR = "nvim";
  VISUAL = "nvim";

  GOPATH = "$HOME/Coding/Go";
  QT_QPA_PLATFORMTHEME = "gtk2";
  RUST_BACKTRACE = 1;

  # https://github.com/electron/electron/issues/8455
  ELECTRON_FORCE_WINDOW_MENU_BAR = 1;

  # https://bbs.archlinux.org/viewtopic.php?id = 159016
  _JAVA_AWT_WM_NONREPARENTING = 1;

  # `less` colors
  LESS_TERMCAP_md = "[01;31m";
  LESS_TERMCAP_me = "[0m";
  LESS_TERMCAP_se = "[0m";
  LESS_TERMCAP_so = "[01;44;33m";
  LESS_TERMCAP_ue = "[0m";
  LESS_TERMCAP_us = "[01;32m";
}
