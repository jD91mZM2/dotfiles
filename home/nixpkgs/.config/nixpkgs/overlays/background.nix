self: super:

rec {
  my-background = self.runCommand "my-background.jpg" {} ''
    cp "${self.adapta-backgrounds}/share/backgrounds/adapta/tealized.jpg" "$out"
  '';
  my-background-focus = self.runCommand "my-background-focus.jpg" {} ''
    ${self.imagemagick}/bin/convert -blur 0x3 -fill black -colorize 50% "${my-background}" "$out"
  '';
}
