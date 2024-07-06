{
  description = "Python development environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:

    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        my-python-packages = with pkgs; (ps: with ps; [
          pip
        ]);
      in
      with pkgs;
      {
        devShells.default = mkShell
          {
            name = "python";
            packages = [ (python3.withPackages my-python-packages) ];

            #shellHook = ''
            #  exec fish
            #'';
          };
      });
}
