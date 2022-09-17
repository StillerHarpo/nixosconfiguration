{
  inputs = {
    nixpkgs-newest.url = "github:NixOS/nixpkgs/nixos-22.05";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
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
    nix-doom-emacs = {
      url = "github:nix-community/nix-doom-emacs";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        emacs-overlay.follows = "emacs-overlay";
        flake-compat.follows = "flake-compat";
        flake-utils.follows = "flake-utils";
      };
    };
    nixpkgs-borgbackup.url = "github:StillerHarpo/nixpkgs/borgbackup-restart";
    home-manager-flake = {
      url = "github:nix-community/home-manager/release-22.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nur = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    deploy-rs = {
      url = "github:serokell/deploy-rs";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-compat.follows = "flake-compat";
        utils.follows = "flake-utils";
      };
    };
    darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = {
    self, nixpkgs, home-manager-flake, agenix, darwin
    , emacs-overlay , nix-doom-emacs
    , nixpkgs-newest, nixpkgs-borgbackup
    , nur, nixos-hardware, deploy-rs, ...
  }:

    let
      system = "x86_64-linux";
      mkPkgs = system: pkgs: overlays: pkgs {
        inherit system overlays;
        config.allowUnfree = true;
      };
      mkPkgsLinux = pkgs: overlays: mkPkgs system pkgs overlays;
      pkgs-newest = mkPkgsLinux (import nixpkgs-newest) [];
      steamOverlay =
        _: super: {
          steam = pkgs-newest.steam.override { extraPkgs = pkgs: [ pkgs.libpng pkgs.icu ]; };
        };
      myShellOverlay =
        _: super: {
          myshell = "${super.zsh}/bin/zsh";
        };
      pkgs = mkPkgsLinux (import nixpkgs) [
        (_: super: {
          inherit (pkgs-newest) signal youtube-dl;
          deploy-rs = deploy-rs.defaultPackage."${system}";
          mytexlive = with super; texlive.combine {inherit (texlive) scheme-full pygmentex pgf collection-basic;};
        })
        myShellOverlay
        emacs-overlay.overlay
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

      legacyPackages.x86_64-linux = pkgs;

      nixosConfigurations = {
        nixos-thinkpad = nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = {
            inherit pkgs agenix;
            home-manager-flake = home-manager-flake.nixosModule;
            borgbackup-local = "${nixpkgs-borgbackup}/nixos/modules/services/backup/borgbackup.nix";
          };
          modules = [
            nixpkgs.nixosModules.notDetected
            nixos-hardware.nixosModules.lenovo-thinkpad-t480s
            home-manager-flake.nixosModules.home-manager
            {
              home-manager.users.florian = { pkgs, config, ... }: {
                imports = [ nix-doom-emacs.hmModule ];
                programs.doom-emacs = {
                  enable = true;
                  doomPrivateDir = ./doom.d;
                  emacsPackage = pkgs.emacsPgtkNativeComp;
                  emacsPackagesOverlay = _: super:
                    {
                      ob-ammonite = super.trivialBuild rec {
                        pname = "ob-ammonite";
                        version = "0.0.0";
                        src = pkgs.fetchFromGitHub {
                          owner = "zwild";
                          repo = "ob-ammonite";
                          rev = "39937dff395e70aff76a4224fa49cf2ec6c57cca";
                          sha256 = "0m5rzpqxk7hrbxsgqplkg7h2p7gv6s1miymv5gvw0cz039skaf0s";
                        };
                      };
                    };
                  extraConfig = ''
                      (with-nix-pathes '((nix-zsh-path . "${pkgs.myshell}")
                                        (nix-latexmk-path . "${pkgs.mytexlive}/bin/latexmk")
                                        (nix-mpv-path . "${pkgs.mpv}/bin/mpv")
                                        (nix-jdk-path . "${pkgs.jdk}/bin/java")
                                        (nix-languagetool-path . "${pkgs.languagetool}")))
                    '';
                };
                xdg = {
                  enable = true;
                  configFile."nix/inputs/nixpkgs".source = nixpkgs.outPath;
                };
                home.sessionVariables.NIX_PATH = "nixpkgs=${config.xdg.configHome}/nix/inputs/nixpkgs$\{NIX_PATH:+:$NIX_PATH}";
                nix.registry.nixpkgs.flake = self;
              };
              nix = {
                registry.nixpkgs.flake = self;
                nixPath = [ "nixpkgs=${nixpkgs.outPath}" ];
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

      darwinConfigurations."Florians-MBP" = darwin.lib.darwinSystem
        (let system = "x86_64-darwin";
         in {
           pkgs = mkPkgs system (import nixpkgs) [myShellOverlay];
           modules = [
             ./mac/configuration.nix
             home-manager-flake.darwinModules.home-manager
           ];
         });
    };
}
