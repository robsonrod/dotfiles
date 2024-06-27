{ config, pkgs, ... }:
let
  my-python-packages = ps: with ps; [
    pip
  ];

in
{
  home.packages = with pkgs; [
    (python3.withPackages my-python-packages)
  ];
}
