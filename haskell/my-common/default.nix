{
  mkDerivation,
  base,
  lib,
  protolude,
  text,
  xmonad-contrib,
  aeson,
  turtle,
}:
mkDerivation {
  pname = "my-common";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base
    protolude
    text
    xmonad-contrib
    aeson
    turtle
  ];
  license = "unknown";
}
