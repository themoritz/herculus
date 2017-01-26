{ mkDerivation, fetchhg, aeson, base, ghcjs-base, react-flux, servant
, stdenv, text
}:
mkDerivation {
  pname = "react-flux-servant";
  version = "0.1.0";
  src = fetchhg {
    url = "https://bitbucket.org/rubenmoor/react-flux-servant";
    sha256 = "0crv66lhiwby8vncp9pkh6gdw9pnda8qfz4dmpsvjgrxnkiaxw50";
    rev = "5bdfec97337a";
  };
  libraryHaskellDepends = [
    aeson base ghcjs-base react-flux servant text
  ];
  homepage = "https://bitbucket.org/wuzzeb/react-flux-servant";
  description = "Allow react-flux stores to send requests to a servant server";
  license = stdenv.lib.licenses.bsd3;
}

