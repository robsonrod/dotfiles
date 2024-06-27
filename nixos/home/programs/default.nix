{ config, pkgs, ... }:

{
  imports = [
    ./browsers.nix
    ./cli.nix
    ./fish.nix
    ./git.nix
    ./network-manager.nix
    ./terms.nix
    ./pdfeditor.nix
    ./neovim.nix
  ];
}

