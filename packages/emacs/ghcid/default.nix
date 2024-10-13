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
    url = "https://raw.githubusercontent.com/StillerHarpo/ghcid/fix-emacs-plugin-for-emacs29/plugins/emacs/ghcid.el";
    hash = "sha256-xq5VSJEmdV5FdnUTVo63YVOI4Fv+4SHkunTXzztCSWk=";
  };
}
