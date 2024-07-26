{ config, options, lib, pkgs, ... }:

with lib;
with lib.types;
let
  cfg = config.modules.configfiles;
  dotfiles = "$HOME/dotfiles";
in
{
  options.modules.configfiles = {

    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    home.file.".local/bin" = {
      source = ../../bin/.local/bin;
      recursive = true;
    };

    home.file.".bashrc" = {
      source = ../../bash/.bashrc;
    };

    home.file.".xinitrc" = {
      source = ../../xmanager-fhd/.xinitrc;
    };
    
    home.file.".xinitrc.exwm" = {
      source = ../../xmanager-hdpi/.xinitrc.exwm;
    };

    home.file.".config/bat" = {
      source = ../../bat/.config/bat;
      recursive = true;
    };

    home.file.".config/atuin" = {
      source = ../../atuin/.config/atuin;
      recursive = true;
    };

    home.file.".config/dunst" = {
      source = ../../dunst/.config/dunst;
      recursive = true;
    };

    home.file.".config/picom" = {
      source = ../../picom/.config/picom;
      recursive = true;
    };

    home.file.".config/ranger" = {
      source = ../../ranger/.config/ranger;
      recursive = true;
    };

    home.file.".config/rofi" = {
      source = ../../rofi/.config/rofi;
      recursive = true;
    };

    home.file.".config/terminator" = {
      source = ../../terminator/.config/terminator;
      recursive = true;
    };

    home.file.".config/wallpaper" = {
      source = ../../wallpaper/.config/wallpaper;
      recursive = true;
    };

    home.file.".config/zathura" = {
      source = ../../zathura/.config/zathura;
      recursive = true;
    };

    home.file.".config/starship" = {
      source = ../../starship/.config/starship;
      recursive = true;
    };

    home.activation.linkFiles = config.lib.dag.entryAfter [ "writeBoundary" ] ''
      ln -Tsf ${dotfiles}/bash/.config/bash ~/.config/bash
      ln -Tsf ${dotfiles}/bspwm/.config/bspwm ~/.config/bspwm
      ln -Tsf ${dotfiles}/nvim/.config/nvim ~/.config/nvim
      ln -Tsf ${dotfiles}/polybar/.config/polybar ~/.config/polybar
      ln -Tsf ${dotfiles}/sxhkd/.config/sxhkd ~/.config/sxhkd
      ln -Tsf ${dotfiles}/tmux/.config/tmux ~/.config/tmux
      ln -Tsf ${dotfiles}/emacs/.emacs.d ~/.emacs.d
    '';

  };
}
