# shellcheck shell=bash

version := "0.1.0.0"
# Format code with nixfmt and fourmolu and cabal-fmt
format:
  find . -name '*.nix' -exec nixfmt {} +
  # shellcheck disable=SC2046
  fourmolu --mode inplace $(find . -name '*.hs')
  # shellcheck disable=SC2046
  cabal-fmt --inplace $(find . -name '*.cabal')

docker-image:
  nix bundle --bundler github:NixOS/bundlers#toDockerImage .#merkle-mountain-range
  docker load -i merkle-mountain-range-exe-merkle-mountain-range-0.1.0.0.tar.gz

docker-up:
  docker compose up -d

bundle-merkle-mountain-range:
    rm -f merkle-mountain-range
    nix bundle .#merkle-mountain-range
    cp -L merkle-mountain-range-exe-merkle-mountain-range-arx merkle-mountain-range
    rm merkle-mountain-range-exe-merkle-mountain-range-arx

cachix:
    nix build .#merkle-mountain-range
    cachix push paolino ./result
    nix build .#merkle-mountain-range-tests
    cachix push paolino ./result
    nix bundle .#merkle-mountain-range
    cachix push paolino ./merkle-mountain-range-exe-merkle-mountain-range-arx
    nix bundle --bundler github:NixOS/bundlers#toDockerImage .#merkle-mountain-range
    # shellcheck disable=SC1083
    cachix push paolino ./merkle-mountain-range-exe-merkle-mountain-range-{{version}}.tar.gz

cachix-parallel:
    ( nix build .#merkle-mountain-range -o merkle-mountain-range && \
        cachix push paolino ./merkle-mountain-range)&
    ( nix build .#merkle-mountain-range-tests -o merkle-mountain-range-tests&& \
        cachix push paolino ./merkle-mountain-range-tests)&
    ( nix bundle .#merkle-mountain-range && \
        cachix push paolino ./merkle-mountain-range-exe-merkle-mountain-range-arx)&
    # shellcheck disable=SC1083
    ( nix bundle --bundler github:NixOS/bundlers#toDockerImage .#merkle-mountain-range && \
        cachix push paolino ./merkle-mountain-range-exe-merkle-mountain-range-{{version}}.tar.gz)&
