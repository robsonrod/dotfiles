{ config, pkgs, ... }:

{
  imports = [
    ./browsers.nix
    ./cli.nix
    ./clojure.nix
    ./fish.nix
    ./git.nix
    ./network-manager.nix
    ./terms.nix
    ./pdfeditor.nix
    ./neovim.nix
  ];
}

