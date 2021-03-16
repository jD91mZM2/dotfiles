{ pkgs, config, ... }:
{
  environment.systemPackages = with pkgs; [
    stack
    haskellPackages.haskell-language-server
    haskellPackages.ormolu
  ];

  home.home.file.".stack/config.yaml".text =
    # YAML is a JSON superset
    builtins.toJSON {
      templates.params = {
        "author-name" = config.globals.name;
        "author-email" = config.globals.email;
        "github-username" = config.globals.name;
      };
    };
}
