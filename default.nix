{ mkDerivation, base, pure-core, pure-default, stdenv }:
mkDerivation {
  pname = "pure-suspense";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure-core pure-default ];
  homepage = "github.com/grumply/pure-suspense";
  description = "Suspense decorator";
  license = stdenv.lib.licenses.bsd3;
}
