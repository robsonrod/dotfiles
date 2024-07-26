{ config, options, lib, pkgs, ... }:

with lib;
with lib.types;
let
cfg = config.modules.atuin;
in
{
    options.modules.atuin = {
        enable = mkOption {
            type = bool;
            default = false;
        };
    };

    config = mkIf cfg.enable {
        programs.atuin = {
            enable = true;
            enableFishIntegration = true;
            flags = [
                "--disable-up-arrow"
            ];
        };

    };

}
