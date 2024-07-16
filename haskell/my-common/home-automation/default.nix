{ mkDerivation, aeson, base, lib, net-mqtt, text, time }:
mkDerivation {
  pname = "my-common";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base net-mqtt text time ];
  license = "unknown";
}
