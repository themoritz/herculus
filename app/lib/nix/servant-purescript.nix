{ mkDerivation, aeson, base, bytestring, containers, directory
, filepath, http-types, lens, mainland-pretty, purescript-bridge
, servant, servant-foreign, servant-server, servant-subscriber
, stdenv, text
}:
mkDerivation {
  pname = "servant-purescript";
  version = "0.8.0.0";
  sha256 = "0h2a215i5lqalc4hp0g7iav3nxypdm5i2yfix0i96038sqjzg0wp";
  libraryHaskellDepends = [
    aeson base bytestring containers directory filepath http-types lens
    mainland-pretty purescript-bridge servant servant-foreign
    servant-server servant-subscriber text
  ];
  testHaskellDepends = [
    aeson base containers lens mainland-pretty purescript-bridge
    servant servant-foreign servant-subscriber text
  ];
  homepage = "https://github.com/eskimor/servant-purescript#readme";
  description = "Generate PureScript accessor functions for you servant API";
  license = stdenv.lib.licenses.bsd3;
}
