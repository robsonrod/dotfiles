{ config, pkgs, ... }:

{
  imports = [
    ./browsers.nix
    ./cli.nix
    ./clojure.nix
    ./fish.nix
    ./git.nix
    ./guile.nix
    ./network-manager.nix
    ./rust.nix
    ./terms.nix
    ./python.nix
    ./pdfeditor.nix
    ./neovim.nix
  ];
}

