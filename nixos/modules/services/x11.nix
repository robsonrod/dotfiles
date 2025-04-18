{ config, options, lib, pkgs, ... }:

with lib;
with lib.types;
let cfg = config.modules.services.x11;
in {
  options.modules.services.x11 = {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {

    environment = {
      localBinInPath = true;

      systemPackages = with pkgs; [
        v4l-utils
        alsa-utils
        arandr
        betterlockscreen
        bspwm
        cacert
        curl
        dunst
        feh
        firefox
        font-manager
        git
        libinput-gestures
        libnotify
        networkmanager_dmenu
        nixpkgs-fmt
        ntfsprogs
        pavucontrol
        picom
        playerctl
        polybarFull
        pulsemixer
        rofi
        rofi-calc
        scrot
        sxhkd
        wget
        killall
        openssl
        p7zip
        unzip
        zip
        xorg.xauth
        xorg.xdpyinfo
        xorg.xhost
        xorg.xinit
        xorg.xinput
        xorg.xkill
        xorg.xrandr
        xorg.xset
      ];
    };

    programs = {

      gnupg.agent = {
        enable = true;
        pinentryPackage = pkgs.pinentry-tty;
      };

      thunar = {
        enable = true;
        plugins = with pkgs.xfce; [
          thunar-volman
          thunar-archive-plugin
        ];
      };

      dconf = { enable = true; };

      bash = {
          completion.enable = true; };

      fish = {
        enable = true;
      };
    };

    services = {
      printing = { enable = true; };
      gvfs = { enable = true; };
      tumbler = { enable = true; };

      libinput = {
        enable = true;
        touchpad = {
          naturalScrolling = true;
        };
      };

    };

    services.xserver = {
      xkb = {
        layout = "us";
        variant = "";
      };
      enable = true;
      autorun = false;
      dpi = 200;

      displayManager = {
        startx.enable = true;
      };

      desktopManager = {
        xfce = { enable = true; };
      };

      windowManager = {
        bspwm = {
          enable = true;
        };
      };

      xautolock = {
        enable = true;
        time = 5;
        locker = "${pkgs.betterlockscreen}/bin/betterlockscreen -l blur";
        killtime = 10;
        killer = "/run/current-system/systemd/bin/systemctl suspend";
        extraOptions = [ "-detectsleep" ];
      };
    };

    fonts = {
      fontDir.enable = true;
      packages = with pkgs; [
        font-awesome
        source-code-pro
        fira-code
        emacs-all-the-icons-fonts
        nerd-fonts.terminess-ttf
        nerd-fonts.iosevka
        nerd-fonts.jetbrains-mono
        nerd-fonts.ubuntu
        nerd-fonts.roboto-mono
        nerd-fonts.fira-code
      ];
    };

  };

}
