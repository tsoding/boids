with import <nixpkgs> {}; {
    BoidsEnv = stdenv.mkDerivation {
        name = "BoidsEnv";
        buildInputs = [ ghc stack mesa freeglut ];
        LD_LIBRARY_PATH="${mesa}/lib:${freeglut}/lib";
    };
}
