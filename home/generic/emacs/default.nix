{ pkgs, config, ... }:

with import ./config.nix { inherit pkgs config; };

{
  home.file = if BYTE_COMPILE_CONFIG
              then {
                ".emacs.d/init.elc".source = "${configDir}/init.elc";
                ".emacs.d/my-config".source = configDir;
              }
              else {
                ".emacs.d/init.el".source = "${configDir}/init.el";
                ".emacs.d/my-config".source = configDir;
              };

  programs.emacs = {
    enable = true;
    package = emacs;
    extraPackages = depsForEpkgs;
  };
}
