{ config, pkgs, inputs, ... }:

{
  imports =
    [
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Bootloader.
  boot = {
    loader = {
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot";
      };
      grub = {
        enable = true;
        efiSupport = true;
        useOSProber = true;
        devices = [ "nodev" ];
        configurationLimit = 5;
        gfxmodeEfi = "1920x1080x32";
        darkmatter-theme = {
        enable = true;
        style = "nixos";
      };
      };
    };
  };

  networking = {
    hostName = "iracema";
    networkmanager = {
      enable = true;
      dns = "dnsmasq";
      dhcp = "internal";
    };
  };

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


  modules = {
    flakes.enable = true;

    services = {
      ssh.enable = true;
      x11.enable = true;
      pipewire.enable = true;
      fingerprint.enable = false;
    };

    hardware = {
      bluetooth.enable = true;
      sensors.enable = true;
      intel.enable = true;
    };

  };

  users.users.robson = {
    shell = pkgs.fish;
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
  };

  networking.firewall.enable = false;

  system = {
    stateVersion = "23.05";
    copySystemConfiguration = false;
  };
}
