{
  trivialBuild,
  fetchFromGitHub,
  # all-the-icons,
  lib,
}:
trivialBuild rec {
  pname = "lambda-line";
  version = "main-23-11-2022";
  src = fetchFromGitHub {
    owner = "mickeynp";
    repo = "combobulate";
    rev = "427fca4eba7fad8f9146735a3777aa1b8d4cb3fc";
    hash = lib.fakeHash;
  };
  # elisp dependencies
  # propagatedUserEnvPkgs = [
  #   all-the-icons
  # ];
  # buildInputs = propagatedUserEnvPkgs;
}
