{ lib, ... }:

with lib;

makeOverridable
  (
    { colours }:
    let
      reorder = map (builtins.elemAt colours);
    in
    {
      inherit colours;

      # Obtained from
      # https://github.com/base16-templates/base16-xresources/blob/d762461de45e00c73a514408988345691f727632/templates/default.mustache#L29-L45
      xresources = reorder [ 00 08 11 10 13 14 12 05 03 09 01 02 04 06 15 07 ];
    }
  )
{
  colours = [
    "282936" # base 0 - *background*
    "3a3c4e" # base 1 - lighter bg (status bars)
    "4d4f68" # base 2 - selection bg
    "626483" # base 3 - comments / line-highlight
    "62d6e8" # base 4 - darker fg (status bars)
    "e9e9f4" # base 5 - *foreground*
    "f1f2f8" # base 6 - light fg (rarely used)
    "f7f7fb" # base 7 - light bg (rarely used)
    "ea51b2" # base 8 - variables and xml tags
    "b45bcf" # base 9 - integers, booleans, xml attributes
    "00f769" # base A - classes
    "ebff87" # base B - strings
    "a1efe4" # base C - regexp and escape characters
    "62d6e8" # base D - functions
    "b45bcf" # base E - keywords
    "00f769" # base F - open/close language tags
  ];
}
