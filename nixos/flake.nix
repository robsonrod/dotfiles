{
  description = "My personal NixOS configuration";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "nixpkgs/nixos-23.05";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
    rust-overlay.url = "github:oxalica/rust-overlay";
    nixos-hardware.url = "github:nixos/nixos-hardware";
  };

  outputs = inputs@{ self, nixpkgs, nixpkgs-stable, home-manager, nixos-hardware, ... }:
    let
      cfg = {
        system = "x86_64-linux";
        config = {
          allowUnfree = true;
        };
      };
      pkgs = import nixpkgs cfg;
      hm-config = {
        useGlobalPkgs = true;
        useUserPackages = true;
        extraSpecialArgs = inputs;
      };
    in
    {
      formatter.${cfg.system} = nixpkgs.nixpkgs-fmt;
      nixosConfigurations = {
        iracema = nixpkgs.lib.nixosSystem rec {
          system = cfg.system;
          specialArgs = {
            inherit inputs;

            pkgs-stable = import nixpkgs-stable {
              inherit system;
              config.allowUnfree = true;
            };
          };
          modules = [
            ./hosts/xps
            ./modules/flakes.nix
            ./modules/services/ssh.nix
            ./modules/services/x11.nix
            ./modules/services/pipewire.nix
            ./modules/services/fingerprint.nix
            ./modules/hardware/bluetooth.nix
            ./modules/hardware/intel.nix
            ./modules/hardware/nvidia.nix
            ./modules/hardware/sensors.nix
            ./modules/docker.nix
            nixos-hardware.nixosModules.common-pc-laptop
            nixos-hardware.nixosModules.common-pc-laptop-ssd
            home-manager.nixosModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                extraSpecialArgs = inputs;

                users.robson = {
                  home.stateVersion = "23.05";
                  imports = [
                    ./modules/xresources.nix
                    ./modules/configfiles.nix
                    ./modules/emacs.nix
                    ./modules/atuin.nix
                    ./modules/gtk.nix
                    ./modules/services/gpg.nix
                    ./home
                  ];
                };
              };
            }
          ];
        };
      };

    };
}
