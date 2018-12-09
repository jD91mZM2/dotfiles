{ pkgs, ... }:
{
  enable = true;
  iconTheme = {
    name = "Numix-Circle";
    package = pkgs.numix-icon-theme-circle;
  };
  settings = {
    global = {
      geometry = "300x5-30+20";
      indicate_hidden = true;
      transparency = 20;
      separator_height = 2;
      padding = 8;
      horizontal_padding = 8;
      frame_width = 3;
      frame_color = "#aaaaaa";
      separator_color = "frame";
      sort = true;
      idle_threshold = 120;
      font = "Hack 8";
      markup = "full";
      format = "<b>%s</b>\\n%b";
      alignment = "left";
      show_age_threshold = 60;
      word_wrap = true;
      stack_duplicates = true;
      show_indicators = true;
      icon_position = "left";
      max_icon_size = 32;
      sticky_history = true;
      history_length = 5;
    };
    shortcuts = {
      close = "ctrl+BackSpace";
      close_all = "ctrl+shift+BackSpace";
      history = "ctrl+grave";
      context = "ctrl+shift+period";
    };

    urgency_low = {
      background = "#101010";
      foreground = "#ffffff";
      frame_color = "#101010";
      timeout = 10;
    };
    urgency_normal = {
      background = "#000000";
      foreground = "#ffffff";
      frame_color = "#000000";
      timeout = 10;
    };
    urgency_critical = {
      background = "#900000";
      foreground = "#ffffff";
      frame_color = "#900000";
      timeout = 0;
    };
  };
}
