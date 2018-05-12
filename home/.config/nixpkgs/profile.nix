{
  PATH = "$PATH:$HOME/.cargo/bin:$HOME/.local/bin:$HOME/Coding/Go/bin:$HOME/bin:$HOME/.local/bin:$HOME/redox/deb/usr/bin";

  EDITOR = "nvim";
  GOPATH = "$HOME/Coding/Go";
  QT_QPA_PLATFORMTHEME = "gtk2";
  RUST_BACKTRACE = 1;
  TERMINAL = "konsole";
  VISUAL = "nvim";

  # https://github.com/electron/electron/issues/8455
  ELECTRON_FORCE_WINDOW_MENU_BAR = 1;

  # https://bbs.archlinux.org/viewtopic.php?id = 159016
  _JAVA_AWT_WM_NONREPARENTING = 1;

  # `less` colors
  LESS_TERMCAP_md = "\x1b[01;31m";
  LESS_TERMCAP_me = "\x1b[0m";
  LESS_TERMCAP_se = "\x1b[0m";
  LESS_TERMCAP_so = "\x1b[01;44;33m";
  LESS_TERMCAP_ue = "\x1b[0m";
  LESS_TERMCAP_us = "\x1b[01;32m";
}
