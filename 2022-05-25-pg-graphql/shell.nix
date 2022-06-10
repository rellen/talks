{ pkgs ? import <nixpkgs> { }, nixpkgs ? <nixpkgs> }:
let
    inherit (pkgs.lib) optional optionals;

in pkgs.mkShell rec {
  name = "pg_graphql_talk";
  buildInputs = with pkgs; [
    postgresql_14
   ];

}
