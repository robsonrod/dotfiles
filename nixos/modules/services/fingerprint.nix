{ config, options, lib, pkgs, ... }:

with lib;
with lib.types;
let cfg = config.modules.services.fingerprint;
in {
  options.modules.services.fingerprint = {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    security.pam.services = {
      login.fprintAuth = true;
      xscreensaver.fprintAuth = true;
    };

    services.fprintd.enable = true;
  };
}
