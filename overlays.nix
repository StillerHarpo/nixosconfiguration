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
    inherit (import ./packages/scripts final inputs) my-linkopen my-linkopenwithx rpi4-install deck-install my-mediamenu;
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
    rpi4-uefi-firmware = final.callPackage ./packages/rpi4-uefi-firmware {};
    emacsPackagesFor = emacs: (
      (prev.emacsPackagesFor emacs).overrideScope (_: eprev: {
        ghcid = final.callPackage ./packages/emacs/ghcid { inherit (eprev) trivialBuild; };
        lean4-mode = final.callPackage ./packages/emacs/lean4-mode { inherit (eprev) trivialBuild dash flycheck lsp-mode magit-section
       ; };
        my-nix-paths = final.callPackage ./packages/emacs/my-nix-paths { inherit (eprev) trivialBuild; };
      })
    );
  };
  emacs = inputs.emacs-overlay.overlay;
  nur = inputs.nur.overlays.default;
}
