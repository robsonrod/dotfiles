{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    xdg-user-dirs
  ];

  home.file.".config" = {
    source = ../../xdg-dirs/.config;
    recursive = true;
  };
}
