self: super:
{
  superTuxKart = super.superTuxKart.overrideAttrs (old: {
    name = "supertuxkart-overlay";
    version = "1.0.0";

    srcs = [
      (self.fetchFromGitHub {
          owner = "supertuxkart";
          repo = "stk-code";
          rev = "deba8d389955a7b2183d32f096aa990b337c314c";
          sha256 = "03mrnzrvfdgjc687n718f5zsray6vbdlv4irzy2mfi78bz3bkjll";
          name = "stk-code";
      })
      (builtins.elemAt old.srcs 1)
    ];
    patches = []; # patch no longer needed
  });
  superTuxKart-editor = self.stdenv.mkDerivation {
    name = "supertux-editor-overlay";
    src = self.fetchFromGitHub {
        owner = "supertuxkart";
        repo = "stk-editor";
        rev = "bb1405f5d05b909f62f79089327c8bddcec1de0f";
        sha256 = "1pmmnjw30dd0w69xvv2aws3brvihrb4ji2axdl9qcr9d01bbmgfl";
    };

    buildInputs = with self; [ cmake libGL physfs x11 xorg.libXxf86vm zlib ];
    installPhase = ''
      ls supertuxkart-editor || true
      ls -la
      mkdir -p $out/bin
      install supertuxkart-editor -m0755 $out/bin
    '';

    meta = {
      description = "SuperTuxKart - Track Editor";
      homepage = https://supertuxkart.net/Track_Editor;
      license = self.stdenv.lib.licenses.mit;
      maintainers = [];
    };
  };
}
