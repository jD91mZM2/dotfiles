{ pkgs, ... }:

{
  environment.systemPackages = [
    pkgs.thunderbird

    (pkgs.makeAutostartItem {
      name = "thunderbird";
      package = pkgs.thunderbird;
    })
  ];
}
