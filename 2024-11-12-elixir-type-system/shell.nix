{ pkgs ? import <nixpkgs> { }, nixpkgs ? <nixpkgs> }:
let
    inherit (pkgs.lib) optional optionals;
    erlang = pkgs.beam.interpreters.erlang_27;
    #elixir = pkgs.beam.packages.erlang_27.elixir_1_18;
    # Custom Elixir package
    elixir = pkgs.beam.packages.erlang_27.elixir.overrideAttrs (oldAttrs: {
      version = "1.18.0-dev";
      src = pkgs.fetchFromGitHub {
        owner = "elixir-lang";
        repo = "elixir";
        # Replace with desired commit hash
        rev = "main";
        sha256 = "sha256-8pbAD49FkdD0k+DgvkvBBGbjIJ90CYsIoXqDmp1sNbQ=";
      };
    });

in pkgs.mkShell rec {
  name = "1.18 R27";
  buildInputs = with pkgs; [
    (texlive.combine { inherit (texlive) scheme-full; })
    typst
    typst-lsp
    typstyle
    rebar
    rebar3
    erlang
    elixir
    elixir_ls
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
