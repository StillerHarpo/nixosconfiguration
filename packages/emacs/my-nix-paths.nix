{
  trivialBuild,
  writeText,
  myshell,
  mytexlive,
  mpv,
  my-linkopenwithx,
  jdk,
  languagetool,
  ...
}:
trivialBuild rec {
  pname = "my-nix-paths";
  version = "local";
  src = writeText "my-nix-paths.el" ''
    (setq nix-zsh-path "${myshell}"
          nix-latexmk-path "${mytexlive}/bin/latexmk"
          nix-mpv-path "${mpv}/bin/mpv"
          nix-linkopenwithx-path "${my-linkopenwithx}/bin/linkopenwithx"
          nix-jdk-path "${jdk}/bin/java"
          nix-languagetool-path "${languagetool}")

    (provide 'my-nix-paths)
  '';
}
