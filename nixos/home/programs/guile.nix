{ config, pkgs, ... }:
{
  home.packages = with pkgs; [
    guile
  ];

}
