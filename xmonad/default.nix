{ stdenv, haskellPackages }:

let
  env = haskellPackages.ghcWithPackages (p: with p; [
    happy
    ghc-mod
    hlint
    hoogle
    structured-haskell-mode
    hasktags
    present
    stylish-haskell


    xmonad
    xmonad-contrib
    xmonad-extras
    random
    split
  ]);
in
  stdenv.mkDerivation {
    name        = "xmonad.hs";
    buildInputs = [env];
    shellHook   = ''
      export NIX_GHC="${env}/bin/ghc"
      export NIX_GHCPKG="${env}/bin/ghc-pkg"
      export NIX_GHC_DOCDIR="${env}/share/doc/ghc/html"
      export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
    '';
  }
