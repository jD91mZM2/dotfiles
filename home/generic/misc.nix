{ pkgs, shared, ... }:
{
  programs.scaff = {
    enable = true;
    imports = let
      baseURL = "https://gitlab.com/jD91mZM2/scaff-repo/-/jobs/782984841/artifacts/raw/build";
      rawConfig = builtins.fetchurl {
        url = "${baseURL}/config.toml";
        sha256 = "06cfijsjipha4521pvq0ng8h7dlamvfgwi7dgjypiab1gz11nfsl";
      };
      config = builtins.fromTOML (builtins.readFile rawConfig);
    in pkgs.lib.mapAttrs (_: value:
      "${baseURL}/${value}"
    ) config.imports;
  };

  programs.mpv = {
    enable = true;
    config = {
      keep-open = true;
    };
  };

  home.file.".tmux.conf".source = ./misc/tmux.conf;
  home.file.".editorconfig".source = ./misc/default.editorconfig;
  home.file.".gdbinit".source = ./misc/gdbinit;

  home.file.".stack/config.yaml".text =
    builtins.toJSON { # YAML is a JSON superset
      templates.params = {
        "author-name" = shared.consts.name;
        "author-email" = shared.consts.email;
        "github-username" = shared.consts.name;
      };
    };
}
