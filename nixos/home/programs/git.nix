{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    delta
  ];
  programs.git = {
    enable = true;
  };

  home.file.".config/git" = {
    source = ../../../git/.config/git;
    recursive = true;
  };

}
