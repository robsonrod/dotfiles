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
    neofetch
    peek
    pinentry
    ranger
    ripgrep
    starship
    stow
    xdg-utils
    zoxide
  ];
}
