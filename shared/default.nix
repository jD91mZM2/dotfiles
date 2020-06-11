{
  pkgs ? import <nixpkgs> {},
  callPackage ? pkgs.callPackage,
  ...
}:

rec {
  consts   = import ./consts.nix;
  builders = callPackage ./builders {};
  theme    = import ./theme.nix;

  scripts = pkgs.runCommand "scripts" {} ''
    cp -r ${./scripts} "$out"
    for f in $out/*; do
      substituteInPlace "$f" \
        ${toString
          (map
            (color: ''
              --subst-var-by "base${color.hex}" "${color.rgb}" \
            '')
            theme.colors)
         }
         # empty line to allow trailing \
    done
  '';

  background = pkgs.runCommand "background.jpg" {} ''
    cp "${pkgs.adapta-backgrounds}/share/backgrounds/adapta/tealized.jpg" "$out"
  '';
}
