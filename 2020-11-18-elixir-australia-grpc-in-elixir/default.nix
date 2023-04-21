{ pkgs ? import ./nix/pkgs.nix {} }:

with pkgs;

stdenv.mkDerivation {
  name = "talk-env";
  buildInputs = [ latexmk ];
  src = ./.;
  buildPhase = "make";
}
