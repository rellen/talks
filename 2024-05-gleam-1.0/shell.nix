{ pkgs ? import <nixpkgs> { }, nixpkgs ? <nixpkgs> }:
let
    inherit (pkgs.lib) optional optionals;
    erlang = pkgs.beam.interpreters.erlangR26;
    elixir = pkgs.beam.packages.erlangR26.elixir_1_16;

in pkgs.mkShell rec {
  name = "gleam and org/beamer";
  buildInputs = with pkgs; [
    pandoc
    (texlive.combine { inherit (texlive) scheme-full; })
    texlab # lsp for tex
    rebar
    rebar3
    erlang
    elixir
    elixir_ls
    gleam
    ] ++ optional stdenv.isLinux inotify-tools ++ optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [ CoreFoundation CoreServices ]);

shellHook = ''
     # this allows mix to work on the local directory
     mkdir -p .nix-mix
     mkdir -p .nix-hex
     export MIX_HOME=$PWD/.nix-mix
     export HEX_HOME=$PWD/.nix-hex
     export PATH=$MIX_HOME/bin:$PATH
     export PATH=$HEX_HOME/bin:$PATH
     export LANG=en_US.UTF-8
     export ERL_AFLAGS="-kernel shell_history enabled"
     export ERL_LIBS=""
   '';
}
