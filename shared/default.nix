{ pkgs, inputs }:

let
  inherit (pkgs) callPackage;
in rec {
  consts   = import ./consts.nix;
  builders = callPackage ./builders { inherit inputs; };
  theme    = callPackage ./theme.nix {};

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

  mkSymlink = path: pkgs.runCommand "symlink-${path}" {} ''
    ln -s "${consts.dotfiles}/${path}" "$out"
  '';

  background = pkgs.runCommand "background.jpg" {} ''
    cp "${pkgs.adapta-backgrounds}/share/backgrounds/adapta/tealized.jpg" "$out"
  '';
}
