{ inputs, outputs, ... }:

{
  aliases = _: prev: { myshell = "${prev.zsh}/bin/zsh"; };
  modifications = final: prev: {
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
    myAspellWithDicts =
      (final.aspellWithDicts (dicts: [
        dicts.de
        dicts.en
        dicts.en-computers
        dicts.en-science
      ]));
    haskellPackages = prev.haskellPackages.extend (_: hPrev: {
      my-common = prev.haskell.lib.overrideCabal
        (hPrev.callPackage ./haskell/my-common { }) (_: {
          prePatch =
            "substituteInPlace Xrandr.hs --replace 'xrandr' '${prev.xorg.xrandr}/bin/xrandr'";
        });
      home-automation = hPrev.callPackage ./haskell/my-common/home-automation { };
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
      (prev.emacsPackagesFor emacs).overrideScope (_: eprev: {
        ghcid = final.callPackage ./packages/emacs/ghcid { inherit (eprev) trivialBuild; };
        lean4-mode = final.callPackage ./packages/emacs/lean4-mode { inherit (eprev) trivialBuild dash flycheck lsp-mode magit-section
       ; };
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
