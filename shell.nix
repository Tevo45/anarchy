with (import <nixpkgs> {});
{ ghc ? haskell.compiler.ghc884 }:

haskell.lib.buildStackProject {
  inherit ghc;
  name = "Anarchy";
  buildInputs = [ gtk3 gobject-introspection ];
}
