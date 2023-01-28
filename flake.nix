{
  inputs = {
    nixpkgs-newest.url = "github:NixOS/nixpkgs/nixos-22.11";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
    doom-emacs = {
      url = "github:doomemacs/doomemacs";
      flake = false;
    };
    nixpkgs-borgbackup.url = "github:StillerHarpo/nixpkgs/borgbackup-restart";
    home-manager-flake = {
      url = "github:nix-community/home-manager/release-22.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nur.url = "github:nix-community/NUR";
    deploy-rs = {
      url = "github:serokell/deploy-rs";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-compat.follows = "flake-compat";
        utils.follows = "flake-utils";
      };
    };
    envfs = {
      url = "github:Mic92/envfs";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = inputs@{ self, nixpkgs, home-manager-flake, agenix, darwin
    , emacs-overlay, doom-emacs, nixpkgs-newest, nixpkgs-borgbackup, nur
    , nixos-hardware, deploy-rs, envfs, ... }:

    let
      system = "x86_64-linux";
      mkPkgs = system: pkgs: overlays:
        pkgs {
          inherit system overlays;
          config.allowUnfree = true;
        };
      mkPkgsLinux = pkgs: overlays: mkPkgs system pkgs overlays;
      pkgs-newest = mkPkgsLinux (import nixpkgs-newest) [ ];
      steamOverlay = _: super: {
        steam = pkgs-newest.steam.override {
          extraPkgs = pkgs: [ pkgs.libpng pkgs.icu ];
        };
      };
      myShellOverlay = _: super: { myshell = "${super.zsh}/bin/zsh"; };
      pkgs = mkPkgsLinux (import nixpkgs) [
        (_: super: {
          inherit (pkgs-newest) signal youtube-dl;
          deploy-rs = deploy-rs.defaultPackage."${system}";
          mytexlive = with super;
            texlive.combine {
              inherit (texlive) scheme-full pygmentex pgf collection-basic;
            };
        })
        myShellOverlay
        emacs-overlay.overlay
        (self: super: rec {
          haskellPackages = super.haskellPackages.extend (_: hSuper: {
            my-common = super.haskell.lib.overrideCabal
              (hSuper.callCabal2nix "my-common" ./haskell/my-common { }) (_: {
                prePatch =
                  "substituteInPlace Xrandr.hs --replace 'xrandr' '${super.xorg.xrandr}/bin/xrandr'";
              });
          });
          monitor-changer = super.writers.writeHaskellBin "monitor-changer" {
            libraries = [ self.haskellPackages.my-common ];
          } ./haskell/monitor-changer/MonitorChanger.hs;
          textcleaner = self.callPackage ./textcleaner { };
          x_wr_timezone = with self.python3Packages;
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
          recurring_ical_events = with self.python3Packages;
            buildPythonPackage rec {
              pname = "recurring_ical_events";
              version = "1.1.0b0";
              src = fetchPypi {
                inherit pname version;
                sha256 = "sha256-kJFnFFSp32I1bJ0OnvZjZeJVxswEBE9mXQg90IpsXIg=";
              };
              propagatedBuildInputs =
                [ python-dateutil icalendar pytz x_wr_timezone ];
              doCheck = false;
            };
          ical2orgpy = with self.python3Packages;
            buildPythonPackage rec {
              pname = "ical2orgpy";
              version = "0.4.0";
              src = fetchPypi {
                inherit pname version;
                sha256 = "sha256-7/kWW1oTSJXPJtN02uIDrFdNJ9ExKRUa3tUNA0oJSoc=";
              };
              propagatedBuildInputs =
                [ click future icalendar pytz tzlocal recurring_ical_events ];
              doCheck = false;

              nativeBuildInputs = [ pbr ];
            };
        })
        steamOverlay
        nur.overlay
      ];

    in {

      legacyPackages.x86_64-linux = pkgs;

      nixosConfigurations = {
        nixos-thinkpad = nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = {
            inherit pkgs agenix inputs;
            home-manager-flake = home-manager-flake.nixosModule;
            borgbackup-local =
              "${nixpkgs-borgbackup}/nixos/modules/services/backup/borgbackup.nix";
          };
          modules = [
            nixpkgs.nixosModules.notDetected
            nixos-hardware.nixosModules.lenovo-thinkpad-t480s
            home-manager-flake.nixosModules.home-manager
            {
              home-manager = {
                users.florian = { pkgs, config, ... }: {
                  xdg = {
                    enable = true;
                    configFile."nix/inputs/nixpkgs".source = nixpkgs.outPath;
                  };
                  home.sessionVariables.NIX_PATH =
                    "nixpkgs=${config.xdg.configHome}/nix/inputs/nixpkgs\${NIX_PATH:+:$NIX_PATH}";
                  nix.registry.nixpkgs.flake = self;
                };
              };
              nix = {
                registry.nixpkgs.flake = self;
                nixPath = [ "nixpkgs=${nixpkgs.outPath}" ];
              };
            }
            ./linux/thinkpad/configuration.nix
            agenix.nixosModules.age
            envfs.nixosModules.envfs
          ];
        };

        desktop = nixpkgs.lib.nixosSystem {
          specialArgs = { inherit pkgs agenix; };
          inherit system;
          modules = with nixos-hardware.nixosModules; [
            nixpkgs.nixosModules.notDetected
            common-pc
            common-pc-hdd
            common-pc-ssd
            common-cpu-amd
            common-gpu-amd-southern-islands
            home-manager-flake.nixosModules.home-manager
            ./linux/desktop/configuration.nix
            agenix.nixosModules.age
          ];
        };
      };

      deploy.nodes.desktop = {
        hostname = "192.168.178.24";
        profiles.system = {
          user = "root";
          sshUser = "root";
          path = deploy-rs.lib.x86_64-linux.activate.nixos
            self.nixosConfigurations.desktop;
        };
      };

      # This is highly advised, and will prevent many possible mistakes
      checks = builtins.mapAttrs
        (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;

      devShells.x86_64-linux.default = pkgs.haskellPackages.developPackage {
        returnShellEnv = true;
        root = ./haskell;
        withHoogle = false;
        modifier = with pkgs;
          with haskellPackages;
          drv:
          haskell.lib.overrideCabal drv (attrs: {
            buildTools = (attrs.buildTools or [ ]) ++ [
              cabal-install
              haskell-language-server
              brittany
              hlint
              xlibsWrapper
            ];
          });
      };

      darwinConfigurations."Florians-MBP" = darwin.lib.darwinSystem
        (let system = "x86_64-darwin";
        in {
          pkgs = mkPkgs system (import nixpkgs) [ myShellOverlay ];
          modules = [
            ./mac/configuration.nix
            home-manager-flake.darwinModules.home-manager
          ];
        });
    };
}
