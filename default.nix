# This file was auto-generated by cabal2nix. Please do NOT edit manually!

{ cabal, logging, monadLogger, optparseApplicative, shelly
, temporary, systemFilepath, text, criterion, aeson
}:

cabal.mkDerivation (self: {
  pname = "list-fusion-lab";
  version = "0.0.1";
  src = builtins.filterSource (path: type: type != "unknown") ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    logging monadLogger optparseApplicative shelly temporary
    systemFilepath text criterion aeson
  ];
  meta = {
    homepage = "https://github.com/nomeata/list-fusion-lab";
    description = "Test suite for benchmarking Data.List implementations";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
