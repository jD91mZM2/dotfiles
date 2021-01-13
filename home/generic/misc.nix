{ pkgs, shared, ... }:
{
  programs.scaff = {
    enable = true;
    extra-configs = "https://gitlab.com/jD91mZM2/scaff-repo/-/jobs/932530814/artifacts/raw/build/config.toml";
  };

  programs.mpv = {
    enable = true;
    config = {
      keep-open = true;
    };
  };

  programs.direnv = {
    enable = true;
    enableNixDirenvIntegration = true;
  };

  home.file.".tmux.conf".source = ./misc/tmux.conf;
  home.file.".gdbinit".source = ./misc/gdbinit;

  # Might wanna update frequently
  home.file.".editorconfig".source = shared.mkSymlink "home/generic/misc/default.editorconfig";

  home.file.".stack/config.yaml".text =
    builtins.toJSON { # YAML is a JSON superset
      templates.params = {
        "author-name" = shared.consts.name;
        "author-email" = shared.consts.email;
        "github-username" = shared.consts.name;
      };
    };
}
