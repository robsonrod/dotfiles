{ config, pkgs, ... }:
{
  programs.kitty = {
    enable = true;
  };

  home.file.".config/kitty" = {
    source = ../../../kitty/.config/kitty;
    recursive = true;
  };
}
