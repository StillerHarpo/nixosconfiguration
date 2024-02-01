{ inputs, outputs, ... }:

{
  aliases = _: prev: { myshell = "${prev.zsh}/bin/zsh"; };
  modifications = final: prev: {
    lesspipe = final.callPackage "${inputs.nixpkgs-lesspipe.outPath}/pkgs/tools/misc/lesspipe" {};
    rclone =
      let
        enableCmount = true;
      in
        with final; buildGoModule
          rec {
            pname = "rclone";
            version = "1.65.0";
            src = fetchFromGitHub {
              owner = pname;
              repo = pname;
              rev = "a752563842b3a603c7d73b00e71c6ff1fd120382";
              hash = "sha256-yds9yWjmfRcStFJQb1MGlXjQxnApFtnkRxb4ZVC7UEI=";
            };
            proxyVendor = true;
            #deleteVendor = true;
            vendorHash = "sha256-BUSI07Ziu2Llu9z1CpREKohqQfGFLgTGWhQ+OA0dABk=";
            subPackages = [ "." ];
            outputs = [ "out" "man" ];
            buildInputs = lib.optional enableCmount (if stdenv.isDarwin then macfuse-stubs else fuse);
            nativeBuildInputs = [ installShellFiles makeWrapper ];
            tags = lib.optionals enableCmount [ "cmount" ];
            ldflags = [ "-s" "-w" "-X github.com/rclone/rclone/fs.Version=${version}" ];
            postInstall =
              let
                rcloneBin =
                  if stdenv.buildPlatform.canExecute stdenv.hostPlatform
                  then "$out"
                  else lib.getBin buildPackages.rclone;
              in
                ''
                  installManPage rclone.1
                  for shell in bash zsh fish; do
                    ${rcloneBin}/bin/rclone genautocomplete $shell rclone.$shell
                    installShellCompletion rclone.$shell
                  done
                '' + lib.optionalString (enableCmount && !stdenv.isDarwin)
                # use --suffix here to ensure we don't shadow /run/wrappers/bin/fusermount,
                # as the setuid wrapper is required as non-root on NixOS.
                ''
                  wrapProgram $out/bin/rclone \
                    --suffix PATH : "${lib.makeBinPath [ fuse ] }" \
                    --prefix LD_LIBRARY_PATH : "${fuse}/lib"
                '';
            passthru.tests = {
              inherit librclone;
            };
          };
    steam = prev.steam.override {
      extraPkgs = pkgs: [
        pkgs.libpng
        pkgs.icu
      ];
    };
    mytexlive = with prev;
      texlive.combine {
        inherit (texlive) scheme-full pygmentex pgf collection-basic;
      };
  };
  additions = final: prev: {
    inherit (import ./packages/scripts final inputs) my-linkopen my-linkopenwithx rpi4-install deck-install;
    haskellPackages = prev.haskellPackages.extend (_: hPrev: {
      my-common = prev.haskell.lib.overrideCabal
        (hPrev.callCabal2nix "my-common" ./haskell/my-common { }) (_: {
          prePatch =
            "substituteInPlace Xrandr.hs --replace 'xrandr' '${prev.xorg.xrandr}/bin/xrandr'";
        });
    });
    monitor-changer = prev.writers.writeHaskellBin "monitor-changer" {
      libraries = [ final.haskellPackages.my-common ];
    } ./haskell/monitor-changer/MonitorChanger.hs;
    textcleaner = final.callPackage ./textcleaner { };
    x_wr_timezone = with final.python3Packages;
      buildPythonPackage rec {
        pname = "x_wr_timezone";
        version = "0.0.5";
        src = fetchPypi {
          inherit pname version;
          sha256 = "sha256-wFyzS5tYpGB6eI2whtyuV2ZyjkuU4GcocNxVk6bhP+Y=";
        };
        propagatedBuildInputs = [ icalendar pytz ];
        doCheck = false;
      };
    recurring_ical_events = with final.python3Packages;
      buildPythonPackage rec {
        pname = "recurring_ical_events";
        version = "1.1.0b0";
        src = fetchPypi {
          inherit pname version;
          sha256 = "sha256-kJFnFFSp32I1bJ0OnvZjZeJVxswEBE9mXQg90IpsXIg=";
        };
        propagatedBuildInputs =
          [ python-dateutil icalendar pytz final.x_wr_timezone ];
        doCheck = false;
      };
    ical2orgpy = with final.python3Packages;
      buildPythonPackage rec {
        pname = "ical2orgpy";
        version = "0.4.0";
        src = fetchPypi {
          inherit pname version;
          sha256 = "sha256-7/kWW1oTSJXPJtN02uIDrFdNJ9ExKRUa3tUNA0oJSoc=";
        };
        propagatedBuildInputs =
          [ click future icalendar pytz tzlocal final.recurring_ical_events ];
        doCheck = false;

        nativeBuildInputs = [ pbr ];
      };
    rpi4-uefi-firmware = final.callPackage ./packages/rpi4-uefi-firmware {};
    wofi-pass = final.callPackage "${inputs.nixpkgs-unstable.outPath}/pkgs/tools/security/pass/wofi-pass.nix" {};
    emacsPackagesFor = emacs: (
      (prev.emacsPackagesFor emacs).overrideScope' (_: eprev: {
        ghcid = final.callPackage ./packages/emacs/ghcid { inherit (eprev) trivialBuild; };
        my-nix-paths = final.callPackage ./packages/emacs/my-nix-paths { inherit (eprev) trivialBuild; };
      })
    );
    slack-wayland = prev.slack.overrideAttrs (oldAttrs: rec {
      fixupPhase = ''
        sed -i -e 's/,"WebRTCPipeWireCapturer"/,"LebRTCPipeWireCapturer"/' $out/lib/slack/resources/app.asar
        rm $out/bin/slack
        makeWrapper $out/lib/slack/slack $out/bin/slack \
          --prefix XDG_DATA_DIRS : $GSETTINGS_SCHEMAS_PATH \
          --suffix PATH : ${final.lib.makeBinPath [ final.xdg-utils ]} \
          --add-flags "--ozone-platform-hint=auto --enable-features=WaylandWindowDecorations,WebRTCPipeWireCapturer"
      '';
});
  };
  emacs = inputs.emacs-overlay.overlay;
  nur = inputs.nur.overlay;
}
