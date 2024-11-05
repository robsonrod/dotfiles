{ config, options, lib, pkgs, ... }:

with lib;
with lib.types;
let cfg = config.modules.services.pipewire;
in {
  options.modules.services.pipewire = {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    security.rtkit.enable = true;

    services.pipewire = {
      audio.enable = true;
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;
      wireplumber.enable = true;
    };

  };
}
