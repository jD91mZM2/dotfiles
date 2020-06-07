{ pkgs, ... }:

with import ./config.nix { inherit pkgs; };

{
  home.file = {
    ".emacs.d/init.elc".source = "${configDir}/init.elc";
    ".emacs.d/my-config".source = configDir;
  };

  programs.emacs = {
    enable = true;
    extraPackages = depsForEpkgs;
  };
}
