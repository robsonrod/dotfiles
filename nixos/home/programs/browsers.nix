{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
      nyxt
  ];

  programs = {

    google-chrome = {
      enable = true;
    };

    firefox = {
      enable = true;
    };

  };
}
