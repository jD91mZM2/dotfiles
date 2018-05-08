[(
  self: super:
  {
    discord = super.discord.overrideAttrs (orig: rec {
      name = "discord-${version}";
      version = "0.0.5";

      src = super.fetchurl {
          url = "https://cdn.discordapp.com/apps/linux/${version}/${name}.tar.gz";
          sha256 = "2f4464bcea532673ca7b314dc2a1b7966f5d175e535a0254753f778dc559ef18";
      };
    });
  }
)]
