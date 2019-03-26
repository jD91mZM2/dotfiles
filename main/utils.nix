{ lib }:
{
  cleanSource = src: let
      abs = rel: builtins.toString src + ("/" + rel);
    in lib.cleanSource (lib.cleanSourceWith {
      filter = (path: type: !(lib.hasPrefix (abs "target") path) && !(lib.hasSuffix ".sqlite" path));
      inherit src;
    });
}
