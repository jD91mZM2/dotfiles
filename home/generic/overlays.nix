map
  (name: import (./overlays + "/${name}"))
  (builtins.attrNames (builtins.readDir ./overlays))
