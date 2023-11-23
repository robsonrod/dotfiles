# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Bootloader.
  boot = {
    loader = {
      grub = {
        enable = true;
        efiSupport = true;
        useOSProber = true;
        devices = [ "nodev" ];
        configurationLimit = 5;
      };
      efi = { canTouchEfiVariables = true; };
    };
  };

  nix = {
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 4d";
    };
  };

  environment = {
    localBinInPath = true;
  };

  networking = {
    hostName = "nixos"; # Define your hostname.
    networkmanager = {
      enable = true;
      dns = "dnsmasq";
    };
  };
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Set your time zone.
  time.timeZone = "America/Sao_Paulo";

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_ADDRESS = "pt_BR.UTF-8";
      LC_IDENTIFICATION = "pt_BR.UTF-8";
      LC_MEASUREMENT = "pt_BR.UTF-8";
      LC_MONETARY = "pt_BR.UTF-8";
      LC_NAME = "pt_BR.UTF-8";
      LC_NUMERIC = "pt_BR.UTF-8";
      LC_PAPER = "pt_BR.UTF-8";
      LC_TELEPHONE = "pt_BR.UTF-8";
      LC_TIME = "pt_BR.UTF-8";
    };
  };

  # Enable the X11 windowing system.
  services = {
    xserver = {
      layout = "us";
      xkbVariant = "";
      enable = true;
      autorun = false;

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

    };

    printing = { enable = true; };

    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      # If you want to use JACK applications, uncomment this
      jack.enable = true;

      # use the example session manager (no others are packaged yet so this is enabled by default,
      # no need to redefine it in your config for now)
      #media-session.enable = true;
    };

    openssh = { enable = true; };

    gvfs = { enable = true; };
    tumbler = { enable = true; };

  };

  # Enable sound with pipewire.
  sound.enable = true;
  hardware = {
    pulseaudio.enable = false;
    bluetooth.enable = true;
  };

  security.rtkit.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.robson = {
    isNormalUser = true;
    description = "robson";
    extraGroups = [
      "networkmanager"
      "wheel"
      "video"
      "audio"
      "vboxusers"
      "input"
      "sound"
      "docker"
    ];

    packages = with pkgs; [
      starship
      kitty
      grc
      stow
      exa
      bat
      zoxide
      delta
      ripgrep
      nixpkgs-fmt
    ];
  };

  # Allow unfree packages
  nixpkgs = {
    config = {
      allowUnfree = true;
    };
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment = {
    systemPackages = with pkgs; [
      ((emacsPackagesFor emacs29).emacsWithPackages (epkgs: [ epkgs.vterm ]))
      xorg.xdpyinfo
      xorg.xrandr
      xorg.xauth
      xorg.xhost
      xorg.xinit
      xorg.xkill
      xorg.xinput
      xorg.xset
      python3
      networkmanager_dmenu
      arandr
      cacert
      binutils
      debootstrap
      gnumake
      ntfsprogs
      pulsemixer
      firefox-devedition
      playerctl
      git
      wget
      curl
      bspwm
      polybarFull
      sxhkd
      rofi
      dunst
      cmake
      libnotify
      font-manager
      libinput-gestures
      feh
      peek
      pavucontrol
      flameshot
    ];
  };
  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };

    thunar = {
      enable = true;
      plugins = with pkgs.xfce; [
        thunar-volman
        thunar-archive-plugin
      ];
    };

    fish = {
      enable = true;
    };

    dconf = { enable = true; };

    bash = { enableCompletion = true; };

  };

  fonts = {
    fontDir.enable = true;
    fonts = with pkgs; [
      font-awesome
      source-code-pro
      fira-code
      (nerdfonts.override { fonts = [ "Terminus" "Iosevka" "JetBrainsMono" "Ubuntu" "RobotoMono" "FiraCode" ]; })
    ];
  };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?

}
