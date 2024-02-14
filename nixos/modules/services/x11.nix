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
        pinentryFlavor = "tty";

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
      dpi = 200;

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
        greeters.mini = {
          enable = true;
          user = "robson";
          extraConfig = ''
                [greeter]
                show-password-label = true
                show-input-cursor = true
                invalid-password-text = "Sorry, try again!"
                password-alignment = left
                password-input-width = 20
		show-sys-info = true
                [greeter-theme]
		background-image = ""
		text-color = "#F8F8F2"
    		error-color = "#FF5555"
                background-color = "#282A36"
                window-color = "#FFB86C"
                border-color = "#FFB86C"
                layout-space = 20
                font-size = 16px
                password-background-color = "#44475A"
                password-border-width = 2px
                password-border-color = "#44475A"
                password-border-radius = 4px
            '';
        };
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
