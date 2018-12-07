{ config, pkgs, ... }:
{
  # Intel microcode
  hardware.cpu.intel.updateMicrocode = true;

  # Graphic
  hardware.opengl.driSupport32Bit = true;
  hardware.nvidia.modesetting.enable = true;
  hardware.nvidia.optimus_prime = {
    enable = true;
    nvidiaBusId = "PCI:4:0:0";
    intelBusId = "PCI:0:2:0";
  };

  # Sound
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;
  sound.enable = true;
}
