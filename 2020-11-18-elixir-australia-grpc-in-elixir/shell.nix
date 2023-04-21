{ pkgs ? import ./nix/pkgs.nix {} }:

with pkgs;

let
  inherit (lib) optional optionals;
  erlang = beam.interpreters.erlangR23;
  elixir = beam.packages.erlangR23.elixir_1_11;
in
mkShell {
   buildInputs = [ inkscape rebar rebar3 erlang elixir git nodejs ]
     ++ optional stdenv.isLinux inotify-tools
     ++ optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [
      CoreFoundation
      CoreServices
    ]);
     shellHook = ''
   '';
 }


