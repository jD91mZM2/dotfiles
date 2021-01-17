{ pkgs, inputs }:

let
  inherit (pkgs) callPackage;
in
rec {
  consts = import ./consts.nix;
  builders = callPackage ./builders { inherit inputs; };
  theme = callPackage ./theme.nix { inherit inputs; };

  scripts = pkgs.runCommand "scripts" { } ''
    cp -r ${./new-dotfiles/modules/x11/scripts} "$out"
    for f in $out/*; do
      substituteInPlace "$f" \
        ${toString
          (map
            (colour: ''
              --subst-var-by "base${colour.hex}" "${colour.rgb}" \
            '')
            theme.colours)
         }
         # empty line to allow trailing \
    done
  '';

  mkSymlink = path: pkgs.runCommand "symlink-${path}" { } ''
    ln -s "${consts.dotfiles}/${path}" "$out"
  '';

  background = pkgs.runCommand "background.jpg" { } ''
    cp "${pkgs.adapta-backgrounds}/share/backgrounds/adapta/tealized.jpg" "$out"
  '';
}
