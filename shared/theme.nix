let
  # Colors from https://github.com/tilal6991/base16-onedark-scheme
  self = {
    colors = [
      "282c34" # base 0 - *background*
      "353b45" # base 1 - lighter bg (status bars)
      "3e4451" # base 2 - selection bg
      "545862" # base 3 - comments / line-highlight
      "565c64" # base 4 - darker fg (status bars)
      "abb2bf" # base 5 - *foreground*
      "b6bdca" # base 6 - light fg (rarely used)
      "c8ccd4" # base 7 - light bg (rarely used)
      "e06c75" # base 8 - variables and xml tags
      "d19a66" # base 9 - integers, booleans, xml attributes
      "e5c07b" # base A - classes
      "98c379" # base B - strings
      "56b6c2" # base C - regexp and escape characters
      "61afef" # base D - functions
      "c678dd" # base E - keywords
      "be5046" # base F - open/close language tags
    ];

    getColor = i: builtins.elemAt self.colors i;

    map = f: map (index: f {
      inherit index;
      hex =
        if index < 10
        then toString index
        else builtins.elemAt [ "A" "B" "C" "D" "E" "F" ] (index - 10);
      rgb = self.getColor index;
    }) (builtins.genList (i: i) 16);

    mapStr = f: toString (self.map f);
  };
in self
