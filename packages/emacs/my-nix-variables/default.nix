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
  is-normal-work-bookmark,
  private,
  lib,
  ...
}:
trivialBuild {
  pname = "my-nix-variables";
  version = "local";
  src = writeText "my-nix-variables.el" ''
    (setq nix-zsh-path "${myshell}"
          nix-latexmk-path "${mytexlive}/bin/latexmk"
          nix-mpv-path "${mpv}/bin/mpv"
          nix-linkopen-path "${
            if isWayland then my-linkopen-wayland-or-tty else my-linkopen-x-or-tty
          }/bin/linkopen"
          nix-is-normal-work-bookmark-path "${lib.getExe is-normal-work-bookmark}"
          nix-jdk-path "${jdk}/bin/java"
          nix-languagetool-path "${languagetool}"
          nix-workmail "${private.workMail}"
          nix-workmail-alt "${private.workMailAlt}"
          nix-workmail-old "${private.workMailOld}"
          nix-workmail-signature "${lib.replaceStrings [ "\n" ] [ "\\n" ] private.workSignature}")

    (provide 'my-nix-variables)
  '';
}
