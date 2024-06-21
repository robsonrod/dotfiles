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
    hardware.graphics = {
      enable = true;
    };

    services.xserver.videoDrivers = [ "modesetting" ];
  };
}
