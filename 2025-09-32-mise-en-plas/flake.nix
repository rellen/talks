{
  description = "prenterm";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/master";
  inputs.treefmt-nix.url = "github:numtide/treefmt-nix";

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      treefmt-nix,
    }@inputs:
    flake-utils.lib.eachDefaultSystem (
      system:
      let

        pkgs = import nixpkgs {
          inherit system;
          config = {
            allowUnfree = true;
          };
        };
        inherit (pkgs.lib) optional optionals;

        treefmtEval = treefmt-nix.lib.evalModule pkgs ./treefmt.nix;

      in
      {

        formatter = treefmtEval.config.build.wrapper;

        devShells.default =
          with pkgs;
          mkShell {
            buildInputs =
              [
                presenterm
              ]
              ++ optional stdenv.isLinux inotify-tools;
          };

      }
    );
}
