{ python3Packages, }:

# We build this python package using setuptools because I couldn't seem to get
# poetry2nix to produce a working package (as opposed to application)
python3Packages.buildPythonPackage {
  name = "vim_helpers";
  src = ./.;
}
