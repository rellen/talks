{ pkgs ? import <nixpkgs> { }, nixpkgs ? <nixpkgs> }:
let
    inherit (pkgs.lib) optional optionals;

in pkgs.mkShell rec {
  name = "Erlang Ecosystem for Functional Programmers talk";
  buildInputs = with pkgs; [
    pandoc
    (texlive.combine { inherit (texlive) scheme-full; })
    texlab # lsp for tex 
   ];

}
