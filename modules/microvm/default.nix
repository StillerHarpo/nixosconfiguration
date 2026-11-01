{ lib, ... }:
{
  imports =
    with builtins;
    map (x: ./. + "/${x}") (
      filter (x: x != "default.nix" && x != "secrets" && lib.hasSuffix ".nix" x) (attrNames (readDir ./.))
    );
}
