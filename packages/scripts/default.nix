pkgs: inputs:

let
  makeScript = name: substitutions:
    (pkgs.writeScriptBin name (builtins.readFile ./${name}.sh)).overrideAttrs
      (old: {
        buildCommand = ''
        ${old.buildCommand}
        patchShebangs $out
      '' + pkgs.lib.strings.concatMapStringsSep "\n" (substitution:
        "substituteInPlace $out/bin/${name} --replace '${substitution.pattern}'  '${substitution.replacement}'")
        substitutions;
      });
  mkSubstitute = pkgName: {
    pattern = "${pkgName}";
    replacement = "${pkgs."${pkgName}"}/bin/${pkgName}";
  };
  substitutes = {
    display = {
      pattern = "display";
      replacement = "${pkgs.imagemagick}/bin/display";
    };
  } // pkgs.lib.listToAttrs (map (pkgName: {
    name = "${pkgName}";
    value = mkSubstitute "${pkgName}";
  }) [
    "mpv"
    "feh"
    "curl"
    "wget"
    "zathura"
    "you-get"
    "tuir"
    "firefox"
    "fbv"
    "w3m"
    "pass"
  ]);
  my-linkopenwithoutx =
    makeScript "linkopenwithoutx" (with substitutes; [ mpv wget fbv ]);
in rec {
  my-linkopenwithx = makeScript "linkopenwithx" (with substitutes; [
    mpv
    curl
    feh
    display
    wget
    zathura
    you-get
    tuir
    firefox
  ]);
  my-linkopen = makeScript "linkopen" [
    {
      pattern = "./linkopenwithx.sh";
      replacement = "${my-linkopenwithx}/bin/linkopenwithx";
    }
    {
      pattern = "./linkopenwithoutx.sh";
      replacement = "${my-linkopenwithoutx}/bin/linkopenwithoutx";
    }
  ];
  rpi4-install = makeScript "rpi4-install" ([substitutes.pass] ++ [
    {
      pattern = "nixos-anywhere";
      replacement = "${inputs.nixos-anywhere.packages.x86_64-linux.nixos-anywhere}/bin/nixos-anywhere";
    }
    {
      pattern = "nixosRpi4IP";
      replacement = (import ../../variables.nix).nixosRpi4IP;
    }
    { pattern = "rpi4-uefi-firmware";
      replacement = pkgs.rpi4-uefi-firmware;
    }
  ]);

}
