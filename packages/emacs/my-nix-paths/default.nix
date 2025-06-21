{
  trivialBuild,
  writeText,
  myshell,
  mytexlive,
  mpv,
  jdk,
  languagetool,
  isWayland ? true,
  my-linkopen-wayland-or-tty,
  my-linkopen-x-or-tty,
  ...
}:
trivialBuild {
  pname = "my-nix-paths";
  version = "local";
  src = writeText "my-nix-paths.el" ''
    (setq nix-zsh-path "${myshell}"
          nix-latexmk-path "${mytexlive}/bin/latexmk"
          nix-mpv-path "${mpv}/bin/mpv"
          nix-linkopen-path "${if isWayland then my-linkopen-wayland-or-tty else my-linkopen-x-or-tty}/bin/linkopen"
          nix-jdk-path "${jdk}/bin/java"
          nix-languagetool-path "${languagetool}")

    (provide 'my-nix-paths)
  '';
}
