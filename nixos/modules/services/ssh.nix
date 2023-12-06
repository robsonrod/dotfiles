{ config, options, lib, pkgs, ... }:

with lib;
with lib.types;
let cfg = config.modules.services.ssh;
in {
  options.modules.services.ssh = {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    services.openssh = {
      enable = true;
      settings = {
        PermitRootLogin = "no";
        PasswordAuthentication = false;
      };
    };
  };
}
