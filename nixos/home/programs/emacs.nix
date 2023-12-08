{ config, lib, pkgs, ... }:
let
  emacsPackage = (pkgs.emacsPackagesFor pkgs.emacs-unstable).emacsWithPackages (epkgs: [ epkgs.vterm ]);
in
{
  programs = {
    emacs = {
      enable = true;
      package = emacsPackage;
    };
  };
}
