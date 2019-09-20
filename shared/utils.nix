{ lib }:

{
  cleanSource = (
    let
      excludes = [
        ''^(.*\/)?target(\/.*)?$''
        ''^.*\.sqlite$''
      ];
      shouldKeep = path: lib.all (regex: builtins.match regex path == null) excludes;
    in src: lib.cleanSource (
      lib.cleanSourceWith {
        filter = path: _type: shouldKeep path;
        inherit src;
      }
    )
  );
}
