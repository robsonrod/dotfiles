{ config, options, lib, pkgs, ... }:

with lib;
with lib.types;
let cfg = config.modules.hardware.sensors;
in {
  options.modules.hardware.sensors = {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ lm_sensors ];
  };
}
