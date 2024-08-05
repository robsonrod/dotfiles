{ config, pkgs, ... }:
{
  programs.kitty = {
    enable = true;
  };

  home.file.".config/kitty" = {
    source = ../../../kitty/.config/kitty;
    recursive = true;
  };

  programs.urxvt = {
  enable = true;
  extraConfig = {
    "perl-ext" = "default,tabbedex";
  };
};
  
}
