{ config, pkgs, ... }:
{
  imports = [
    ./programs
    ./xdg-dirs.nix
  ];

  home = {
    username = "robson";
    homeDirectory = "/home/robson";
  };

  programs.home-manager.enable = true;
  modules = {
    xresources.enable = true;
    gtk = {
      enable = true;
      theme = "Dracula";
    };
    configfiles.enable = true;
    emacs.enable = true;
    services.gpg.enable = true;
  };
}

