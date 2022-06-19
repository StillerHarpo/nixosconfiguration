{
  inputs = {
    nixpkgs-newest.url = "github:NixOS/nixpkgs/nixos-22.05";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
    nixos-hardware.url = github:NixOS/nixos-hardware/master;
    agenix.url = "github:ryantm/agenix";
    emacs-overlay.url = "github:nix-community/emacs-overlay/master";
    doom-emacs = {
      url = "github:hlissner/doom-emacs/develop";
      flake = false;
    };
    nix-doom-emacs = {
      url = "github:nix-community/nix-doom-emacs/master";
      inputs = {
        doom-emacs.follows = "doom-emacs";
        nixpkgs.follows = "nixpkgs";
        emacs-overlay.follows = "emacs-overlay";
      };
    };
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-master.url = "github:NixOS/nixpkgs/master";
    nixpkgs-borgbackup.url = "github:StillerHarpo/nixpkgs/borgbackup-restart";
    home-manager = {
      url = "github:nix-community/home-manager/release-22.05";
      inputs = { nixpkgs.follows = "nixpkgs"; };
    };
    nur.url = "github:nix-community/NUR";
  };
  outputs = {
    self, nixpkgs, home-manager, nixpkgs-unstable, agenix
    , emacs-overlay , doom-emacs, nix-doom-emacs
    , nixpkgs-master, nixpkgs-newest, nixpkgs-borgbackup
    , nur, nixos-hardware
  }:

    let
      system = "x86_64-linux";
      mkPkgs = pkgs: overlays: pkgs {
        inherit system overlays;
        config.allowUnfree = true;
      };
      pkgs-unstable = mkPkgs (import nixpkgs-unstable) [];
      pkgs-master = mkPkgs (import nixpkgs-master) [];
      pkgs-newest = mkPkgs (import nixpkgs-newest) [];
      steamOverlay =
        _: super: with pkgs-unstable; {
          steam = pkgs-newest.steam.override { extraPkgs = pkgs: [ pkgs.libpng pkgs.icu ]; };
        };
      pkgs = mkPkgs (import nixpkgs) [
        (_: super: with pkgs-unstable; {
          inherit sane-drivers sane-backends xsane hplip;
          inherit (pkgs-newest) signal;
          python3Packages = super.python3Packages // {inherit (pkgs-master.python3Packages) gunicorn; };
        })
        steamOverlay
        nur.overlay
      ];

    in {
      nixosConfigurations.nixos-thinkpad = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = {
          inherit pkgs pkgs-unstable agenix;
          pkgs-master = mkPkgs (import nixpkgs-master) [];
          borgbackup-local = "${nixpkgs-borgbackup}/nixos/modules/services/backup/borgbackup.nix";
          sane-unstable = "${nixpkgs-unstable}/nixos/modules/services/hardware/sane.nix";
          defaultShell = "zsh";
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
      nixopsConfigurations.default = {
        inherit nixpkgs;
        network.storage.memory = {};
        desktop =
          { config, pkgs, ... }:
          {
            imports = with nixos-hardware.nixosModules; [
              home-manager.nixosModules.home-manager
              common-pc
              common-pc-hdd
              common-pc-ssd
              common-cpu-amd
              common-gpu-amd-southern-islands
              agenix.nixosModules.age
              ./linux/desktop/configuration.nix
            ];
            deployment.targetHost = "192.168.178.24";
            nixpkgs.config = {
              allowUnfree = true;
              overlays = [ steamOverlay ];
            };
          };
      };
      devShell.x86_64-linux = pkgs.haskellPackages.developPackage {
        returnShellEnv = true;
        root = ./.;
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
