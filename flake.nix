{
  inputs = {
    nixpkgs-newest.url = "github:NixOS/nixpkgs/nixos-22.05";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
    nixpkgs-old.url = "github:NixOS/nixpkgs?rev=d17a56d90ecbd1b8fc908d49598fb854ef188461";
    nixos-hardware.url = github:NixOS/nixos-hardware/master;
    agenix.url = "github:ryantm/agenix";
    emacs-overlay.url = "github:nix-community/emacs-overlay?rev=e007354fcc0f492878d85b85334ab3baa08a273b";
    doom-emacs = {
      url = "github:hlissner/doom-emacs?rev=35865ef5e89442e3809b8095199977053dd4210f";
      flake = false;
    };
    nix-doom-emacs = {
      url = "github:nix-community/nix-doom-emacs?rev=6860a32b4bb158db85371efd7df0fe35ebcecb9b";
      inputs = {
        doom-emacs.follows = "doom-emacs";
        nixpkgs.follows = "nixpkgs-old";
        emacs-overlay.follows = "emacs-overlay";
      };
    };
    nixpkgs-borgbackup.url = "github:StillerHarpo/nixpkgs/borgbackup-restart";
    home-manager = {
      url = "github:nix-community/home-manager/release-22.05";
      inputs = { nixpkgs.follows = "nixpkgs"; };
    };
    nur.url = "github:nix-community/NUR";
    deploy-rs = {
      url = "github:serokell/deploy-rs";
      inputs = { nixpkgs.follows = "nixpkgs"; };
    };
  };
  outputs = {
    self, nixpkgs, home-manager, agenix
    , emacs-overlay , doom-emacs, nix-doom-emacs
    , nixpkgs-newest, nixpkgs-borgbackup
    , nur, nixos-hardware, deploy-rs, ...
  }:

    let
      system = "x86_64-linux";
      mkPkgs = pkgs: overlays: pkgs {
        inherit system overlays;
        config.allowUnfree = true;
      };
      pkgs-newest = mkPkgs (import nixpkgs-newest) [];
      steamOverlay =
        _: super: {
          steam = pkgs-newest.steam.override { extraPkgs = pkgs: [ pkgs.libpng pkgs.icu ]; };
        };
      pkgs = mkPkgs (import nixpkgs) [
        (_: super: {
          inherit (pkgs-newest) signal youtube-dl;
          deploy-rs = deploy-rs.defaultPackage."${system}";
        })
        (self: super: {
          haskellPackages = super.haskellPackages.extend (_: hSuper: {
            my-common =
              super.haskell.lib.overrideCabal
                (hSuper.callCabal2nix "my-common" ./haskell/my-common {})
                (_: { prePatch =
                        "substituteInPlace Xrandr.hs --replace 'xrandr' '${super.xorg.xrandr}/bin/xrandr'" ;
                    });
          });
          monitor-changer =
            super.writers.writeHaskellBin
              "monitor-changer"
              {
                libraries = [self.haskellPackages.my-common];
              }
              ./haskell/monitor-changer/MonitorChanger.hs;
        })
        steamOverlay
        nur.overlay
      ];

    in {
      nixosConfigurations = {
        nixos-thinkpad = nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = {
            inherit pkgs agenix;
            borgbackup-local = "${nixpkgs-borgbackup}/nixos/modules/services/backup/borgbackup.nix";
            defaultShell = "${pkgs.zsh}/bin/zsh";
          };
          modules = [
            nixpkgs.nixosModules.notDetected
            nixos-hardware.nixosModules.lenovo-thinkpad-t480s
            home-manager.nixosModules.home-manager
            {
              home-manager.users.florian = { pkgs, ... }: {
                imports = [ nix-doom-emacs.hmModule ];
                programs.doom-emacs = {
                  enable = true;
                  doomPrivateDir = ./doom.d;
                };
              };
            }
            ./linux/thinkpad/configuration.nix
            agenix.nixosModules.age
          ];
        };
        desktop = nixpkgs.lib.nixosSystem {
          specialArgs = {
            inherit pkgs agenix;
          };
          inherit system;
          modules = with nixos-hardware.nixosModules; [
            nixpkgs.nixosModules.notDetected
            common-pc
            common-pc-hdd
            common-pc-ssd
            common-cpu-amd
            common-gpu-amd-southern-islands
            home-manager.nixosModules.home-manager
            ./linux/desktop/configuration.nix
            agenix.nixosModules.age
          ];
        };

      deploy.nodes.desktop = {
        hostname = "192.168.178.24";
        profiles.system = {
          user = "root";
          sshUser = "root";
          path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.desktop;
        };
      };

      # This is highly advised, and will prevent many possible mistakes
      checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;

      devShells.x86_64-linux.default = pkgs.haskellPackages.developPackage {
        returnShellEnv = true;
        root = ./haskell;
        withHoogle = false;
        modifier = with pkgs; with haskellPackages; drv:
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

    };
}
