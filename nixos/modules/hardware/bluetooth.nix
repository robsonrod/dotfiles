{ config, options, lib, pkgs, ... }:

with lib;
with lib.types;
let cfg = config.modules.hardware.bluetooth;
in {
  options.modules.hardware.bluetooth = {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {

    hardware = {
      pulseaudio.enable = false;
      enableRedistributableFirmware = true;

      bluetooth = {
        enable = true;
        settings = {
          General = {
            Enable = "Source,Sink,Media,Socket";
            MultiProfile = "multiple";
          };
        };
      };

    };

    services.blueman.enable = true;

  };

}
