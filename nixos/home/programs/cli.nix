{ config, lib, pkgs, ... }:
{
  home.packages = with pkgs; [
    bat
    bc
    bottom
    curl
    eza
    fd
    file
    fzf
    grc
    hexyl
    hwinfo
    killall
    libnotify
    lshw
    man-pages
    neofetch
    openssl
    p7zip
    peek
    pinentry
    ranger
    ripgrep
    ripgrep
    starship
    stow
    unzip
    wget
    xdg-utils
    zip
    zoxide
    remmina
  ];
}
