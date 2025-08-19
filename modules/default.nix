{ ... }:
{
  imports = with builtins; map (x: ./. + "/${x}") (filter (x: x != "default.nix" && x != "secrets") (attrNames (readDir ./.)));
}

