self: super:

rec {
  background = self.runCommand "background.jpg" {} ''
    cp "${self.adapta-backgrounds}/share/backgrounds/adapta/tealized.jpg" "$out"
  '';
}
