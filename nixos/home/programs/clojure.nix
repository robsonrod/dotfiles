{ config, pkgs, ... }:
{
  home.packages = with pkgs; [
    (leiningen.override { jdk = jdk17; })
  ];

}
