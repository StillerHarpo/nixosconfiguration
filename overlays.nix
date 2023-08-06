{ inputs, outputs, ... }:

{
  aliases = _: prev: { myshell = "${prev.zsh}/bin/zsh"; };
  modifications = final: prev: {
    steam = prev.steam.override {
      extraPkgs = pkgs: [
        inputs.nixpkgs2211.legacyPackages.${final.system}.openssl
        pkgs.libpng
        pkgs.icu
      ];
    };
    mytexlive = with prev;
      texlive.combine {
        inherit (texlive) scheme-full pygmentex pgf collection-basic;
      };
    myemacs = (prev.emacsPackagesFor prev.emacsUnstable).emacsWithPackages
      (epkgs: [ epkgs.vterm ]);
    # https://github.com/DanBloomberg/leptonica/issues/659
    leptonica = prev.leptonica.overrideAttrs (oldAttrs: {
      patches = (if oldAttrs ? patches then oldAttrs.patches else [ ]) ++ [
        (prev.fetchpatch {
          url =
            "https://github.com/DanBloomberg/leptonica/commit/544561af6944425a284a6bc387d64662501c560e.patch";
          hash = "sha256-rgpXAylSvCJYt4fbUELomfJz3OytsMdeJhcr7neP4yY=";
        })
      ];
    });
  };
  additions = final: prev: {
    inherit (import ./packages/scripts prev) my-linkopen my-linkopenwithx;
    deploy-rs = inputs.deploy-rs.defaultPackage."${final.system}";
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
    native-emacs = let
      emacsPgks = import inputs.nixpkgs {
        overlays = [ inputs.emacs-overlay-unpinned.overlay ];
        system = final.system;
      };
    in emacsPgks.writeScriptBin "native-emacs" ''
      ${
        emacsPgks.emacsWithPackagesFromUsePackage {
          config = ./config.el;
          defaultInitFile = true;
          alwaysEnsure = true;
          package = emacsPgks.emacs-unstable;
          extraEmacsPackages = epkgs:
            [epkgs.treesit-grammars.with-all-grammars];
          override = _: eprev: rec {
            my-nix-paths = prev.callPackage ./packages/emacs/my-nix-paths.nix {
              inherit (eprev) trivialBuild;
            };
            combobulate = prev.callPackage ./packages/emacs/combobulate.nix {
              inherit (prev) fetchFromGitHub;
              inherit (eprev) trivialBuild;
            };
          };
        }
      }/bin/emacs -q --eval '(load-file "~/nixosconfiguration/config.el")'
    '';
  };
  emacs = inputs.emacs-overlay.overlay;
  nur = inputs.nur.overlay;
  nix-alien = inputs.nix-alien.overlay;
}
