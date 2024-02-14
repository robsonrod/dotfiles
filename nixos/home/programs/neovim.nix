{ config, lib, pkgs, ... }:
{
  programs = {
    neovim = {
      enable = true;
      package = pkgs.neovim-nightly;
    };
  };
}
