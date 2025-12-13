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
    mpv = prev.mpv-unwrapped.wrapper { mpv = prev.mpv-unwrapped.override { sdl2Support = true; }; };
    pass = prev.pass.override {
      dmenu = prev.writeScriptBin "dmenu" ''${prev.rofi}/bin/rofi -dmenu "$@"'';
    };
  };
  additions = final: prev: {
    inherit
      (import ./packages/scripts final inputs)
      my-linkopen-wayland-or-tty my-linkopen-x-or-tty rpi4-install deck-install;
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
      net-mqtt = final.haskell.lib.doJailbreak (hPrev.net-mqtt.overrideAttrs (attrs: { meta = attrs.meta // { broken = false; }; }));
    });
    monitor-changer = prev.writers.writeHaskellBin "monitor-changer" {
      libraries = [ final.haskellPackages.my-common ];
    } ./haskell/monitor-changer/MonitorChanger.hs;
    textcleaner = final.callPackage ./textcleaner { };
    rpi4-uefi-firmware = final.callPackage ./packages/rpi4-uefi-firmware {};
    emacsPackagesFor = emacs: (
      (prev.emacsPackagesFor emacs).overrideScope (_: eprev: {
        ghcid = final.callPackage ./packages/emacs/ghcid { inherit (eprev) trivialBuild; };
        lean4-mode = final.callPackage ./packages/emacs/lean4-mode { inherit (eprev) trivialBuild dash flycheck lsp-mode magit-section
       ; };
        eglot-booster = final.callPackage ./packages/emacs/eglot-booster { inherit (eprev) trivialBuild; };
        my-nix-variables = final.callPackage ./packages/emacs/my-nix-variables {
          inherit (eprev) trivialBuild;
          private = prev.private or (import ./fakePrivate.nix);
        };
        pr-review = eprev.pr-review.overrideAttrs (attrs: {
          nativeBuildInputs = (attrs.nativeBuildInputs or []) ++ [final.git];
        });
      })
    );
  };
  emacs = inputs.emacs-overlay.overlay;
  nur = inputs.nur.overlays.default;
}
