{ config, options, lib, pkgs, pkgs-stable, ... }:

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
    systemd = {
      user.services.polkit-gnome-authentication-agent-1 = {
        description = "polkit-gnome-authentication-agent-1";
        wantedBy = [ "graphical-session.target" ];
        wants = [ "graphical-session.target" ];
        after = [ "graphical-session.target" ];
        serviceConfig = {
          Type = "simple";
          ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
          Restart = "on-failure";
          RestartSec = 1;
          TimeoutStopSec = 10;
        };
      };
    };

    security.pam.services = {
      betterlockscreen.fprintAuth = true;
      login.fprintAuth = true;
      sudo.fprintAuth = true;
      polkit.fprintAuth = true;
    };

    services.fprintd = {
      enable = true;
    };
  };
}
