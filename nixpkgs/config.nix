{pkgs}:
let fast = p: pkgs.haskell.lib.dontHaddock (pkgs.haskell.lib.dontCheck p);
in
{
   haskellPackageOverrides = with pkgs.haskell.lib; self: super: {
     ghc-syb-utils = fast super.ghc-syb-utils;
   };
}
