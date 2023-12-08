{ config, options, lib, pkgs, ... }:

with lib;
with lib.types;
let cfg = config.modules.hardware.intel;
in {
  options.modules.hardware.intel = {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    hardware.opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
      extraPackages = with pkgs; [
        intel-compute-runtime
        intel-media-driver
        libvdpau-va-gl
        vaapiIntel
        vaapiVdpau
      ];
    };

    services.xserver.videoDrivers = [ "intel" "nvidia" ];
  };
}
