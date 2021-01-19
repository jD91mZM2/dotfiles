{ config, ... }:
{
  home.programs.git = {
    enable = true;
    lfs.enable = true;

    aliases = {
      mr = "!f() { git fetch \${2-origin} merge-requests/\${1?}/head && git switch -d FETCH_HEAD; }; f";
      pr = "!f() { git fetch \${2-origin} pull/\${1?}/head && git switch -d FETCH_HEAD; }; f";
    };
    extraConfig = {
      pull.rebase = true;
    };
  };
}
