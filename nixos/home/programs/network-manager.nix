{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    networkmanager_dmenu
  ];

  home.file.".config/networkmanager-dmenu" = {
    source = ../../../networkmanager-dmenu/.config/networkmanager-dmenu;
    recursive = true;
  };

}
