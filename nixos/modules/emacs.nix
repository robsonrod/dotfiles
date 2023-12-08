{ config, options, lib, pkgs, ... }:

with lib;
with lib.types;
let
  cfg = config.modules.emacs;
  emacsPackage = (pkgs.emacsPackagesFor pkgs.emacs-unstable).emacsWithPackages (epkgs: [ epkgs.vterm ]);
in
{
  options.modules.emacs = {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    programs = {
      emacs = {
        enable = true;
        package = emacsPackage;
      };
    };

  };

}
