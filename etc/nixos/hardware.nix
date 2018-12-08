{ config, pkgs, ... }:
{
  # Intel microcode
  hardware.cpu.intel.updateMicrocode = true;

  # Graphic
  hardware.opengl.driSupport32Bit = true;
  hardware.bumblebee.enable = true;

  # Sound
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;
  sound.enable = true;
}
