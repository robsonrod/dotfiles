{ config, lib, pkgs, ... }:
{
  home.packages = with pkgs; [
    bat
    bc
    bottom
    eza
    fd
    file
    fzf
    grc
    hexyl
    hwinfo
    libnotify
    lshw
    man-pages
    nerdfetch
    peek
    pinentry
    yazi
    ripgrep
    starship
    stow
    xdg-utils
    zoxide
  ];
}
