{
  services.picom = {
    enable = true;
    backend = "glx";
    experimentalBackends = true;

    fade = true;
    fadeDelta = 5;
    inactiveOpacity = 0.8;
    shadow = true;

    settings = {
      blur = {
        method = "gaussian";
        size = 10;
        deviation = 2.0;
      };
    };
  };
}
