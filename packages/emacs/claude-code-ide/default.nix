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
    rev = "50a3d55262805d7207889ed429ff30da96fbf68b";
    hash = "sha256-u+87PjLh0Mc7C8nvDG758rdgpmjED8G/hO+FE1tj7DU=";
  };
  packageRequires = [
    websocket
    web-server
  ];
}
