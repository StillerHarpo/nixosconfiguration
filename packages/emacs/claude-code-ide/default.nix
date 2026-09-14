{
  trivialBuild,
  pkgs,
  websocket,
  web-server,
  ...
}:
trivialBuild {
  pname = "claude-code-ide";
  version = "master";
  src = pkgs.fetchFromGitHub {
    owner = "manzaltu";
    repo = "claude-code-ide.el";
    rev = "32a8a904ac21e52c54231b99796d12ae36df9c22";
    hash = "sha256-LlJn5lbJpN/fHLggy8KmOAFt3abcG0oZAahruuxCSgM=";
  };
  packageRequires = [
    websocket
    web-server
  ];
}
