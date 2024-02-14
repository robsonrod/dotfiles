{ config, options, lib, pkgs, inputs, ... }:

with lib;
with lib.types;
let
  cfg = config.modules.flakes;
in
{
  options.modules.flakes = {

    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    nix = {
      package = pkgs.nixUnstable;
      gc = {
        automatic = true;
        dates = "weekly";
        options = "--delete-older-than 7d";
      };

      settings = {
        experimental-features = [ "nix-command" "flakes" ];
        allowed-users = [ "robson" ];
        trusted-users = [ "root" "@wheel" ];
        keep-outputs = true;
        auto-optimise-store = true;
        keep-derivations = true;
      };
      optimise.automatic = true;
    };

    nixpkgs = {
      config = { allowUnfree = true; };
      overlays = [
        inputs.emacs-overlay.overlay
        inputs.neovim-nightly-overlay.overlay
        inputs.rust-overlay.overlays.default
      ];
    };

  };
}
