{ config, options, lib, pkgs, ... }:

with lib;
with lib.types;
let cfg = config.modules.services.gpg;
in {
  options.modules.services.gpg = {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    services.gpg-agent = {
      enable = true;
      enableSshSupport = true;
      pinentryFlavor = "tty";
      defaultCacheTtl = 1800;
    };
  };
}
