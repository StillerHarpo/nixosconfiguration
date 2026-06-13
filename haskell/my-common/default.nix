{
  mkDerivation,
  base,
  lib,
  protolude,
  text,
  xmonad-contrib,
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
  ];
  license = "unknown";
}
