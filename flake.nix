{
  inputs = {
    nixpkgs-newest.url = "github:NixOS/nixpkgs/nixos-21.11";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
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
      url = "github:nix-community/home-manager/release-21.11";
      inputs = { nixpkgs.follows = "nixpkgs"; };
    };
  };
  outputs = {
    self, nixpkgs, home-manager, nixpkgs-unstable, agenix
    , emacs-overlay , doom-emacs, nix-doom-emacs
    , nixpkgs-master, nixpkgs-newest, nixpkgs-borgbackup
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
      pkgs = mkPkgs (import nixpkgs) [
        (_: super: with pkgs-unstable; {
          inherit sane-drivers sane-backends xsane hplip;
          inherit (pkgs-newest) signal;
          python3Packages = super.python3Packages // {inherit (pkgs-master.python3Packages) gunicorn; };
          steam = pkgs-newest.steam.override { extraPkgs = pkgs: [ pkgs.libpng pkgs.icu ]; };
        })
      ];

    in {
      nixosConfigurations.nixos-thinkpad = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = {
          inherit pkgs pkgs-unstable agenix;
          pkgs-master = mkPkgs (import nixpkgs-master) [];
          borgbackup-local = "${nixpkgs-borgbackup}/nixos/modules/services/backup/borgbackup.nix";
          sane-unstable = "${nixpkgs-unstable}/nixos/modules/services/hardware/sane.nix";
        };
        modules = [
          nixpkgs.nixosModules.notDetected
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
          ./thinkpadSpecific.nix
          agenix.nixosModules.age
        ];
      };
      nixopsConfigurations.default = {
        inherit nixpkgs;
        network.storage.memory = {};
        desktop =
          { config, pkgs, ... }:
          {
            imports = [ ./desktopSpecific.nix ];
            deployment.targetHost = "192.168.178.24";
          };
      };
      devShell.x86_64-linux = pkgs.haskellPackages.developPackage {
        returnShellEnv = true;
        root = ./scripts/.;
        withHoogle = false;
        modifier = with pkgs; with haskellPackages; drv:
          haskell.lib.overrideCabal drv (attrs: {
            buildTools = (attrs.buildTools or [ ]) ++ [
              cabal-install
              haskell-language-server
              brittany
              hlint
            ];
          });
      };

    };
}
