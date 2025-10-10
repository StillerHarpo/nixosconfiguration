{
  trivialBuild,
  pkgs,
  ...
}:
trivialBuild {
  pname = "eglot-booster";
  version = "master";
  src = pkgs.fetchFromGitHub {
    owner = "jdtsmith";
    repo = "eglot-booster";
    rev = "cab7803c4f0adc7fff9da6680f90110674bb7a22";
    hash = "sha256-xUBQrQpw+JZxcqT1fy/8C2tjKwa7sLFHXamBm45Fa4Y=";
  };
}
