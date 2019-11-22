self: super:

rec {
  background = self.runCommand "background.jpg" {} ''
    cp "${self.adapta-backgrounds}/share/backgrounds/adapta/tealized.jpg" "$out"
  '';
  background-focus = self.runCommand "background-focus.jpg" {} ''
    ${self.imagemagick}/bin/convert -blur 0x3 -fill black -colorize 50% "${background}" "$out"
  '';
}
