{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
        fishPlugins.fzf-fish
        fishPlugins.grc
        fishPlugins.sponge
        fishPlugins.z
        fishPlugins.autopair
        fishPlugins.foreign-env
  ];

  home.file.".config/fish" = {
    source = ../../../fish/.config/fish;
    recursive = true;
  };
}
