# Boids in Haskell

## Usage

### NixOS

    $ nix-shell
    $ stack build --extra-include-dirs=$NIX_USER_PROFILE_DIR/include --extra-lib-dirs=$NIX_USER_PROFILE_DIR/lib
    $ stack exec boids-exe
    $ stack test
