with (import <nixpkgs> {});
with haskell.packages.ghc822;
(mkDerivation {
  pname = "agathion";
  version = "1.0.0.0";
  src = ./.;
  buildDepends = [ split brick MissingH scalpel ];
  buildTools = [ cabal-install ];
  license = stdenv.lib.licenses.gp13;
}).env
