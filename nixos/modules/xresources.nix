{ config, options, lib, pkgs, ... }:

with lib;
with lib.types;
let
  cfg = config.modules.xresources;
in
{
  options.modules.xresources = {

    enable = mkOption {
      type = bool;
      default = false;
    };

    dpi = mkOption {
      type = int;
      default = 200;
    };

  };

  config = mkIf cfg.enable {
    xresources = {
      properties = {
        "*.foreground" = "#F8F8F2";
        "*.background" = "#282A36";
        "*.color0" = "#000000";
        "*.color8" = "#4D4D4D";
        "*.color1" = "#FF5555";
        "*.color9" = "#FF6E67";
        "*.color2" = "#50FA7B";
        "*.color10" = "#5AF78E";
        "*.color3" = "#F1FA8C";
        "*.color11" = "#F4F99D";
        "*.color4" = "#BD93F9";
        "*.color12" = "#CAA9FA";
        "*.color5" = "#FF79C6";
        "*.color13" = "#FF92D0";
        "*.color6" = "#8BE9FD";
        "*.color14" = "#9AEDFE";
        "*.color7" = "#BFBFBF";
        "*.color15" = "#E6E6E6";

        "Xcursor.size" = 48;
        "Xcursor.theme" = "Arc-Dark";
        "Xft.dpi" = cfg.dpi;

        "*.antialias" = "true";
        "*.hinting" = "true";
        "*.autohint" = "true";
        "Xft.hintstyle" = "hintslight";
        "Xft.rgba" = "rgb";
        "Xft.lcdfilter" = "lcddefault";
      };
    };
  };
}
