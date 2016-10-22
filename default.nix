with import <nixpkgs> {}; {
    BoidsEnv = stdenv.mkDerivation {
        name = "Boids";
        buildInput = [ ghc stack mesa freeglut ];
        LD_LIBRARY_PATH="${mesa}/lib:${freeglut}/lib";
    };
}
