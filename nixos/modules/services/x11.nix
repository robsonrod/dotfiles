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
        arandr
        betterlockscreen
        binutils
        bspwm
        cacert
        cmake
        curl
        debootstrap
        dunst
        feh
        firefox
        font-manager
        git
        gnumake
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
        scrot
        sqlite
        sxhkd
        wget
        xorg.xauth
        xorg.xdpyinfo
        xorg.xhost
        xorg.xinit
        xorg.xinput
        xorg.xkill
        xorg.xrandr
        xorg.xset
        fishPlugins.fzf-fish
        fishPlugins.grc
        fishPlugins.sponge
        fishPlugins.z
        fishPlugins.autopair
        fishPlugins.foreign-env
      ];
    };

    programs = {

      gnupg.agent = {
        enable = true;
      };

      thunar = {
        enable = true;
        plugins = with pkgs.xfce; [
          thunar-volman
          thunar-archive-plugin
        ];
      };

      dconf = { enable = true; };

      bash = { enableCompletion = true; };

      fish = {
        enable = true;
      };
    };

    services = {
      printing = { enable = true; };
      gvfs = { enable = true; };
      tumbler = { enable = true; };
    };

    services.xserver = {
      layout = "us";
      xkbVariant = "";
      enable = true;
      autorun = true;

      libinput = {
        enable = true;
        touchpad = {
          naturalScrolling = true;
        };
      };

      displayManager = {
        #startx.enable = true;
        lightdm = {
          enable = true;
        };
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
        (nerdfonts.override { fonts = [ "Terminus" "Iosevka" "JetBrainsMono" "Ubuntu" "RobotoMono" "FiraCode" ]; })
      ];
    };

  };

}
