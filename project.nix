{ system, indexState, src, haskell-nix, ... }:
let
  atIndex = { index-state = indexState; };
  shell = { pkgs, ... }: {
    tools = {
      cabal = atIndex;
      cabal-fmt = atIndex;
      haskell-language-server = atIndex;
      hoogle = atIndex;
      fourmolu = atIndex;
      hlint = atIndex;
      ghcid = atIndex;
    };
    withHoogle = true;
    buildInputs = [
      pkgs.gitAndTools.git
    ];
    shellHook = ''
      echo "Entering shell for merkle-mountain-range project"
    '';
  };

  mkProject = ctx@{ lib, pkgs, ... }: {
    name = "merkle-mountain-range";
    compiler-nix-name = "ghc966";
    inherit src;
    shell = shell { inherit pkgs; };
    modules = [ ];

  };
  project = haskell-nix.cabalProject' mkProject;
  packages = let components = project.hsPkgs.merkle-mountain-range.components;
  in {
    inherit project;
    merkle-mountain-range = components.exes.merkle-mountain-range;
    merkle-mountain-range-tests = components.tests.merkle-mountain-range-tests;
  };
in {
  inherit packages;
  devShell = project.shell;
}
