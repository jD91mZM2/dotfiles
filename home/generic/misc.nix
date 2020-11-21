{ pkgs, shared, ... }:
{
  programs.scaff = {
    enable = true;
    extra-configs = "https://gitlab.com/jD91mZM2/scaff-repo/-/jobs/864649703/artifacts/raw/build/config.toml";
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
