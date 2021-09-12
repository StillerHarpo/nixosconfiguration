{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.05";
    agenix.url = "github:ryantm/agenix";
    nix-doom-emacs.url = "github:vlaci/nix-doom-emacs";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-master.url = "github:NixOS/nixpkgs/master";
    home-manager = {
      url = "github:nix-community/home-manager/release-21.05";
      inputs = { nixpkgs.follows = "nixpkgs"; };
    };
  };
  outputs = { self, nixpkgs, home-manager, nixpkgs-unstable, agenix, nix-doom-emacs, nixpkgs-master }:

    let
      system = "x86_64-linux";
      mkPkgs = pkgs: overlays: pkgs {
        inherit system overlays;
        config.allowUnfree = true;
      };
      pkgs-unstable = mkPkgs (import nixpkgs-unstable) [];
      pkgs = mkPkgs (import nixpkgs) [
        (_: _: with pkgs-unstable; {
          inherit sane-drivers sane-backends xsane hplip paperless-ng; })
      ];

    in {
      nixosConfigurations.nixos-thinkpad = nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = {
        inherit pkgs pkgs-unstable agenix;
        pkgs-master = mkPkgs (import nixpkgs-master) [];
        sane-unstable = "${nixpkgs-unstable}/nixos/modules/services/hardware/sane.nix";
        paperless-ng = "${nixpkgs-unstable}/nixos/modules/services/misc/paperless-ng.nix";
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
