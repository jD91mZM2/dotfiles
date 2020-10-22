{ pkgs, config, shared, ... }:
{
  programs.scaff = {
    enable = true;
    imports = let
      baseURL = "https://gitlab.com/jD91mZM2/scaff-repo/-/jobs/782984841/artifacts/raw/build";
      config = builtins.fromTOML (builtins.readFile (builtins.fetchurl "${baseURL}/config.toml"));
    in pkgs.lib.mapAttrs (_: value:
      builtins.fetchurl "${baseURL}/${value}"
    ) config.imports;
  };

  programs.mpv = {
    enable = true;
    config = {
      keep-open = true;
    };
  };

  home.file.".tmux.conf".source = config.lib.file.mkOutOfStoreSymlink ./misc/tmux.conf;
  home.file.".editorconfig".source = config.lib.file.mkOutOfStoreSymlink ./misc/default.editorconfig;
  home.file.".gdbinit".source = config.lib.file.mkOutOfStoreSymlink ./misc/gdbinit;

  home.file.".stack/config.yaml".text =
    builtins.toJSON { # YAML is a JSON superset
      templates.params = {
        "author-name" = shared.consts.name;
        "author-email" = shared.consts.email;
        "github-username" = shared.consts.name;
      };
    };
}
