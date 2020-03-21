let
    project = import ./release.nix;
    pkgs = project.pkgs;

    inherit (pkgs) haskellPackages;
in
    pkgs.stdenv.mkDerivation {
      name = "shell";
      buildInputs = project.app.env.nativeBuildInputs ++ [
        haskellPackages.cabal-install
      ];
      shellHook = ''
        export NIX_GHC="$(which ghc)"
        export NIX_GHCPKG="$(which ghc-pkg)"
        export NIX_GHC_DOCDIR="$NIX_GHC/../../share/doc/ghc/html"
        export NIX_GHC_LIBDIR="$(ghc --print-libdir)"
      '';
    }
