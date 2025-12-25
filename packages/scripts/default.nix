pkgs: inputs:

let
  makeScript =
    name: substitutions:
    (pkgs.writeScriptBin name (builtins.readFile ./${name}.sh)).overrideAttrs (old: {
      buildCommand = ''
        ${old.buildCommand}
        patchShebangs $out
      ''
      + pkgs.lib.strings.concatMapStringsSep "\n" (
        substitution:
        "substituteInPlace $out/bin/${name} --replace '${substitution.pattern}'  '${substitution.replacement}'"
      ) substitutions;
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
    jellytui = {
      pattern = "@jellytui@";
      replacement = "${inputs.jellytui.defaultPackage.x86_64-linux}/bin/jellytui";
    };
  }
  // pkgs.lib.listToAttrs (
    map
      (pkgName: {
        name = "${pkgName}";
        value = mkSubstitute "${pkgName}";
      })
      [
        "mpv"
        "feh"
        "curl"
        "wget"
        "zathura"
        "you-get"
        "tuir"
        "w3m"
        "pass"
        "jq"
        "fzf"
        "fd"
      ]
  );
  my-linkopen-tty = pkgs.resholve.writeScriptBin "linkopentty.sh" {
    interpreter = "${pkgs.bash}/bin/bash";
    inputs = with pkgs; [
      coreutils
      mpv
      wget
      gnugrep
      curl
      gnused
      w3m
    ];
    execer = [
      "cannot:${pkgs.lib.getExe pkgs.wget}"
      "cannot:${pkgs.lib.getExe pkgs.mpv}"
      "cannot:${pkgs.lib.getExe pkgs.feh}"
      "cannot:${pkgs.lib.getExe pkgs.wget}"
      "cannot:${pkgs.lib.getExe pkgs.w3m}"
    ];
  } (builtins.readFile ./linkopentty.sh);
  my-linkopen-desktop =
    inputs: fakes: excers:
    pkgs.resholve.writeScriptBin "linkopendesktop.sh" {
      interpreter = "${pkgs.bash}/bin/bash";
      inputs =
        with pkgs;
        [
          coreutils
          gnugrep
          mpv
          curl
          feh
          imagemagick
          wget
          zathura
          you-get
          tuir
          jq
          w3m
          librewolf
        ]
        ++ inputs;
      fake.external = [ "sudo" ] ++ fakes;
      execer = [
        "cannot:${pkgs.lib.getExe pkgs.you-get}"
        "cannot:${pkgs.lib.getExe pkgs.tuir}"
        "cannot:${pkgs.lib.getExe pkgs.mpv}"
        "cannot:${pkgs.lib.getExe pkgs.feh}"
        "cannot:${pkgs.lib.getExe pkgs.wget}"
        "cannot:${pkgs.lib.getExe pkgs.w3m}"
        "cannot:${pkgs.lib.getExe pkgs.zathura}"
        "cannot:${pkgs.lib.getExe pkgs.librewolf}"
      ]
      ++ excers;
    } (builtins.readFile ./linkopendesktop.sh);
  my-linkopen-x = my-linkopen-desktop [ pkgs.xsel pkgs.xorg.xrandr ] [ "wl-copy" "swaymsg" ] [ ];
  my-linkopen-wayland = my-linkopen-desktop [
    pkgs.wl-clipboard
    pkgs.sway
  ] [ "xsel" "xrandr" ] [ "cannot:${pkgs.lib.getExe' pkgs.sway "swaymsg"}" ];
  my-linkopen-desktop-or-tty =
    linkopendesktop:
    pkgs.resholve.writeScriptBin "linkopen" {
      interpreter = "${pkgs.bash}/bin/bash";
      inputs = [
        pkgs.coreutils
        pkgs.gnugrep
        linkopendesktop
        my-linkopen-tty
      ];
      execer = [
        "cannot:${pkgs.lib.getExe linkopendesktop}"
        "cannot:${pkgs.lib.getExe my-linkopen-tty}"
      ];
    } (builtins.readFile ./linkopen.sh);
in
{
  my-linkopen-wayland-or-tty = my-linkopen-desktop-or-tty my-linkopen-wayland;
  my-linkopen-x-or-tty = my-linkopen-desktop-or-tty my-linkopen-x;
  rpi4-install = makeScript "rpi4-install" (
    [ substitutes.pass ]
    ++ [
      {
        pattern = "nixos-anywhere";
        replacement = "${inputs.nixos-anywhere.packages.x86_64-linux.nixos-anywhere}/bin/nixos-anywhere";
      }
      {
        pattern = "nixosRpi4IP";
        replacement = (import ../../variables.nix).nixosRpi4IP;
      }
      {
        pattern = "rpi4-uefi-firmware";
        replacement = pkgs.rpi4-uefi-firmware;
      }
    ]
  );
  deck-install = makeScript "deck-install" (
    [ substitutes.pass ]
    ++ [
      {
        pattern = "nixos-anywhere";
        replacement = "${inputs.nixos-anywhere.packages.x86_64-linux.nixos-anywhere}/bin/nixos-anywhere";
      }
      {
        pattern = "nixosDeckIP";
        replacement = (import ../../variables.nix).nixosDeckIP;
      }
    ]
  );
  live-streams =
    let
      database = pkgs.fetchurl {
        url = "https://iptv-org.github.io/api/streams.json";
        hash = pkgs.lib.fakeHash;
      };
    in
    pkgs.writeScriptBin "live-streams" ''
      allChannels = ${pkgs.lib.getExe pkgs.jq} 'map(select(.channel)) | map(.channel) | join("\n")' ${database}
      selectedChannel = $(${pkgs.lib.getExe pkgs.fzf} -i $allChannels)
      url = $(${pkgs.lib.getExe pkgs.jq} 'map(select(.channel == $channel)) | .[0] | .url' --raw-output --arg channel $selectedChannel ${database})
      allChannels = $( $(jq 'map(select(.channel)) | map(.channel) | join("\n")'
      ${pkgs.lib.getExe pkgs.mpv} $selectedChannel
    '';

}
