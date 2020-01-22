{
  description = "A really useful tool that does an awesome thing";

  edition = 201909;

  inputs.naersk = {
    url   = "github:nmattia/naersk";
    flake = false;
  };
  inputs.mozilla = {
    url   = "github:mozilla/nixpkgs-mozilla";
    flake = false;
  };
  inputs.jd91mzm2 = {
    url   = "git+https://gitlab.com/jD91mZM2/nur-packages";
    flake = false;
  };

  outputs = { self, nixpkgs, mozilla, naersk, jd91mzm2 }: let
    forAllSystems = nixpkgs.lib.genAttrs [ "x86_64-linux" "x86_64-darwin" "i686-linux" "aarch64-linux" ];
  in {
    # Main package
    packages = forAllSystems (system: {
      my-project = let
        mozillaBuilt  = nixpkgs.legacyPackages."${system}".callPackage "${mozilla}/package-set.nix" {};
        jd91mzm2Built = nixpkgs.legacyPackages."${system}".callPackage jd91mzm2 {};
        naerskBuilt   = nixpkgs.legacyPackages."${system}".callPackage naersk {};

        rust = mozillaBuilt.latest.rustChannels.stable.rust;
      in naerskBuilt.buildPackage {
        name = "my-project";
        src  = jd91mzm2Built.lib.cleanSourceRust ./.;
        root = ./.;

        cargo = rust.cargo;
        rustc = rust.rustc;
      };
    });
    defaultPackage = forAllSystems (system: self.packages."${system}".my-project);

    # Make it runnable with `nix app`
    apps = forAllSystems (system: {
      my-project = {
        type    = "app";
        program = "${self.packages."${system}".my-project}/bin/my-project";
      };
    });
    defaultApp = forAllSystems (system: self.apps."${system}".my-project);
  };
}
