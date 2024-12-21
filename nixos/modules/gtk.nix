{ config, options, lib, pkgs, ... }:

with lib;
with lib.types;
let cfg = config.modules.gtk;
in {
  options.modules.gtk = {
    enable = mkOption {
      type = bool;
      default = false;
    };

    cursor.size = mkOption {
      type = int;
      default = 48;
    };

    cursor.theme = mkOption {
      type = str;
      default = "Adwaita";
    };

    theme = mkOption {
      type = str;
      default = "Arc-Dark";
    };

    icon-theme = mkOption {
      type = str;
      default = "Papirus-Dark";
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      arc-theme
      (papirus-icon-theme.override { color = "grey"; })
      dracula-theme
      dracula-icon-theme
      lxappearance
    ];

    home.file.".config/gtk-3.0/settings.ini" = {
      text = ''
        [Settings]
          gtk-theme-name=${cfg.theme}
          gtk-icon-theme-name=${cfg.icon-theme}
          gtk-font-name=Roboto 11
          gtk-cursor-theme-name=${cfg.cursor.theme}
          gtk-cursor-theme-size=${toString cfg.cursor.size}
          gtk-toolbar-style=GTK_TOOLBAR_BOTH
          gtk-toolbar-icon-size=GTK_ICON_SIZE_LARGE_TOOLBAR
          gtk-button-images=1
          gtk-menu-images=1
          gtk-enable-event-sounds=1
          gtk-enable-input-feedback-sounds=1
          gtk-xft-antialias=1
          gtk-xft-hinting=1
          gtk-xft-hintstyle=hintslight
          gtk-application-prefer-dark-theme=true
          gtk-xft-rgba=rgb
          gtk-key-theme-name = Emacs
      '';
    };

    home.file.".config/gtk-3.0/gtk.css" = {
      text = ''
      VteTerminal,
      vte-terminal,
      TerminalScreen {
          padding: 0px;
          margin: 0px;
          -VteTerminal-inner-border: 0px;
        }
      '';
    };


    home.file.".config/gtk-3.0/bookmarks" = {
      text = ''
        file:///home/robson/Documents
        file:///home/robson/Music
        file:///home/robson/Pictures
        file:///home/robson/Videos
        file:///home/robson/Downloads
      '';
    };

    home.file.".config/gtk-4.0/settings.ini" = {
      text = ''
        [Settings]
        gtk-theme-name=${cfg.theme}
        gtk-icon-theme-name=${cfg.icon-theme}
        gtk-font-name=Roboto 11
        gtk-cursor-theme-name=${cfg.cursor.theme}
        gtk-cursor-theme-size=${toString cfg.cursor.size}
        gtk-menu-images=1
        gtk-enable-event-sounds=1
        gtk-enable-input-feedback-sounds=1
        gtk-xft-antialias=1
        gtk-xft-hinting=1
        gtk-xft-hintstyle=hintfull
        gtk-xft-rgba=rgb
        gtk-application-prefer-dark-theme=true
        gtk-decoration-layout=:menu,appmenu
      '';
    };
  };
}
