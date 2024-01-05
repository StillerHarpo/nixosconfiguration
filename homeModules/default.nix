{ lib, ... }:
{
  imports = with builtins; map (x: ./. + "/${x}") (filter (x: x != "default.nix" && lib.hasSuffix ".nix" x  ) (attrNames (readDir ./.)));
}

