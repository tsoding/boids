# Boids in Haskell

http://www.red3d.com/cwr/boids/

## Usage

### NixOS

    $ nix-shell
    $ stack build --extra-include-dirs=$NIX_USER_PROFILE_DIR/include --extra-lib-dirs=$NIX_USER_PROFILE_DIR/lib
    $ stack exec boids
