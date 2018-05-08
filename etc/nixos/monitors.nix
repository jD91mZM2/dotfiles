{
  services.xserver.xrandrHeads = [
    {
      output = "eDP1";
      monitorConfig = ''
        Option "PreferredMode" "1366x768"
        Option "Position" "0 520"
      '';
    }
    {
      output = "DP1";
      monitorConfig = ''
        Option "PreferredMode" "1280x1024"
        Option "Position" "1366 0"
      '';
    }
  ];
}
