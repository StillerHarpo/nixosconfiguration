{
  trivialBuild,
  fetchurl,
  lib,
  ...
}:
trivialBuild {
  pname = "ghcid";
  version = "master";
  src = fetchurl {
    url = https://raw.githubusercontent.com/ndmitchell/ghcid/master/plugins/emacs/ghcid.el;
    hash = "sha256-b57Setg7hCC6cbKIZAe4xLHWj6HCb4FE8yj5jyZdqC8=";
  };
}
