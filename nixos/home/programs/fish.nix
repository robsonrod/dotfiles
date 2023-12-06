{ config, pkgs, ... }:

{
  home.file.".config/fish" = {
    source = ../../../fish/.config/fish;
    recursive = true;
  };
}
