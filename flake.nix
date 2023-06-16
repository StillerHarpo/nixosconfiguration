{
  inputs = {
    nixpkgs-newest.url = "github:NixOS/nixpkgs/nixos-23.05";
    nixpkgs2211.url = "github:NixOS/nixpkgs/nixos-22.11";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    nix-alien = {
      url = "github:thiagokokada/nix-alien";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
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
      url = "github:nix-community/emacs-overlay?rev=6d54cfde44494da2396678d82bb61eeb0a3fa392";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
    doom-emacs = {
      url =
        "github:doomemacs/doomemacs?rev=d5ccac5d71c819035fa251f01d023b3f94b4fba4";
      flake = false;
    };
    home-manager-flake = {
      url = "github:nix-community/home-manager/release-23.05";
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
    , emacs-overlay, doom-emacs, nixpkgs-newest, nur, nixos-hardware, deploy-rs
    , envfs, nix-alien, nixpkgs2211, ... }:

    let
      system = "x86_64-linux";
      mkPkgs = system: pkgs: overlays:
        pkgs {
          inherit system overlays;
          config.allowUnfree = true;
        };
      mkPkgsLinux = pkgs: overlays: mkPkgs system pkgs overlays;
      pkgs-newest = mkPkgsLinux (import nixpkgs-newest) [ ];
      pkgs2211 = mkPkgsLinux (import nixpkgs2211) [ ];
      steamOverlay = _: super: {
        steam = pkgs-newest.steam.override {
          extraPkgs = pkgs: [ pkgs2211.openssl pkgs.libpng pkgs.icu ];
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
        nix-alien.overlay
        emacs-overlay.overlay
        (self: super: rec {
          myemacs =
            (super.emacsPackagesFor super.emacsUnstable).emacsWithPackages
            (epkgs: [ epkgs.vterm ]);
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
          # https://github.com/DanBloomberg/leptonica/issues/659
          leptonica = super.leptonica.overrideAttrs (oldAttrs: {
            patches = (if oldAttrs ? patches then oldAttrs.patches else [ ])
              ++ [
                (super.fetchpatch {
                  url =
                    "https://github.com/DanBloomberg/leptonica/commit/544561af6944425a284a6bc387d64662501c560e.patch";
                  hash = "sha256-rgpXAylSvCJYt4fbUELomfJz3OytsMdeJhcr7neP4yY=";
                })
              ];
          });
        })
        steamOverlay
        nur.overlay
      ];
      lib = (nixpkgs.lib.extend (_: _: home-manager-flake.lib)).extend
        (import ./mylib);

      thinkpad-modules = [
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
            extraSpecialArgs = {
              lib = lib.extend (_: _: home-manager-flake.lib);
              private = import ./private.nix;
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

      thinkpad-specialArgs = {
        inherit pkgs agenix inputs;
        home-manager-flake = home-manager-flake.nixosModule;
        private = import ./private.nix;
      };
    in {

      legacyPackages.x86_64-linux = pkgs;

      nixosConfigurations = {
        nixosThinkpad = nixpkgs.lib.nixosSystem {
          inherit system lib;
          specialArgs = thinkpad-specialArgs;
          modules = thinkpad-modules;
        };

        thinkpadVM = self.nixosConfigurations.nixosThinkpad.extendModules {
          modules = [
            ({ pkgs, lib, ... }: {
              # mkpasswd password
              users.users.florian.passwordFile = lib.mkForce
                "${pkgs.writeText "password"
                "$y$j9T$DrA2chw40lirPPr5xy/ka0$p36qBLHR8L0bNGDTwVE4RtAw93QTh2WOvtvW4JrpyR7"}";
            })
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

        rpi2 = nixpkgs.lib.nixosSystem {
          system = "armv7l-linux";

          modules = [
            "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-armv7l-multiplatform.nix"
            {
              users.users = {
                root.openssh.authorizedKeys.keys = [
                  "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDIc5ug7o0Jqe6SNNrjW5BAyTWqZGVvehJ7GOZrT7DFiiop174CdDlRo4GZlvAGzFEaWr5/4knS0p8kErDPgcgdfC0IL2BYClPEna8agHhyqvmZISxfFDk48Bg/yGo37iPpuGxT7g6VIqI46PnTqgF3nfX1J3crPDD1tDUv5Nq+LH3qlwnpRA3rMBTym/QPkPAM8jQGB4DtyhI1s6UBEQK5vvljhYBG/P54ILQUokYqIsUirQKpBW7Z3sY+zezJpOc+Y6DRZ0rm9dRa6HsOFQ1DQ6u3FkBcyq+vkr4KWmxDdRO0acAV6o0c+1dqyhdaKfklO1E9ZOScTG9Wur9p17qMPsd1zJ8OZ5S7NDMMFi2wcUkxQO9QNqndo+opOBYVMxrz2Hc2Ch2vzuSlVwUxKE60qfARFZ5ZbVOJ8Ate+vghrldgyRF7Sg5yid8Rv6RHv4nvJZpEFjmtkluWzNwhoaF9ifNdB2y7MZeDgu1n1v9xmexSYs97cCB+sprSLYrqgsk= florian@nixos-thinkpad"
                  "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCg3kPf2YxtGrfckExx/ckZrsXH1sa0mDGAAVaK6VOpLF4s0fnWJmXrBSfeGgOVPHRCNF3dfQR34u5PwO3gtK8FQr8XvVRKP+EcMi3ztKHcawnThJyB2Zi3D8yTGDYnKRXoiDvitTWtDDmZta91QJZsK65R0SeOGSk1lG6MOYyEhTRQS4rV1Ij8qEqAMB7R7/yUIkdoyGDbcDZ05Bs8/NCfBLJ/pv+pRKp+ZFIjqmKNVYznZ4OT7ywbbQaTNUCoQO4Hcm+ujlOw2jV7Xqb9842uBRQKNKik3hGw1DlAxKrbK/s3Uu0Vs/cUrEoH68+tKZdCzoX3YoYI9cyMRS2+LEkb florian@florian"
                  "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDJjjBxd9De7YH9ZCzSLmmBTXYmMOdPaLuToZc5zFe/FUz8V8NhQCmHnPP/wcL2/3gz8jz19PcXKN4jc204Nx+XrFvXPO2BMuaeztwyhLd7o5LLmWEm66PImXq4BPLUH/QTlSumIDPwnehUnorUVEcso2VSBidZ4hor2FTIvP3x3DT19xw+cH19fh6xjSEx3bYk8aQTbXpxQAduou8Q424fmOuaQcCO9f3odYmuhVhmN6hjtCK/NhcKQRQ25C1Ftw+NcC++e2c196J4VLTB3XybKU4BYPI9A1Lq8QHNS16JapGWcvoaamx5o8Br+zxmmBlTJvhMSEUga02QXMrVPkTaDDAwtzSBl1mSD9lx5haCySCH9vsjfR5aoc5Axm1qPiO+kxlLSCmdd7CxexWPR0WbmCt7toPHXGoeVHxY2p6AdeZDdgdr1ccf2iLcE7Y37cwvEBiQx0KCAN3kkYAdSBxFStUDUMtb+cbrnDAqCztg5LIr4GoT5PeXtPW2CCUl74c= root@nixos-thinkpad"
                ];
              };
              system.stateVersion = "22.11";
              boot.supportedFilesystems = pkgs.lib.mkForce [
                "btrfs"
                "reiserfs"
                "vfat"
                "f2fs"
                "xfs"
                "ntfs"
                "cifs"
              ];
              nixpkgs = {
                pkgs = pkgs.pkgsCross.armv7l-hf-multiplatform;
                overlays = [
                  (prev: final: {
                    libxcrypt = final.libxcrypt.overrideAttrs (oldAttrs: {
                      configureFlags = [
                        "--enable-hashes=bcrypt,bcrypt_y,bcrypt_a,bcrypt_x,sha512crypt,sha256crypt,sha1crypt,sunmd5,md5crypt,nt,bsdicrypt,bigcrypt"
                        "--enable-obsolete-api=glibc"
                        "--disable-failure-tokens"
                      ];
                    });
                  })
                ];
              };
            }
          ];
        };
      };

      images.rpi2 = self.nixosConfigurations.rpi2.config.system.build.sdImage;

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
      checks = lib.recursiveUpdate (builtins.mapAttrs
        (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib) {
          x86_64-linux.screenlocker =
            (import "${inputs.nixpkgs}/nixos/lib" { }).runTest {
              name = "screenlocker";
              nodes.machine = { pkgs, lib, ... }: {
                imports = thinkpad-modules;
                age = {
                  identityPaths =
                    lib.mkForce [ "${./linux/thinkpad/test-secrets/id_rsa}" ];
                  secrets = lib.mkForce {
                    florian.file =
                      ./linux/thinkpad/test-secrets/passwordHash.age;
                    paperless.file = ./linux/thinkpad/test-secrets/password.age;
                    birthdate.file =
                      ./linux/thinkpad/test-secrets/birthdate.age;
                  };
                };
              };
              node.specialArgs = thinkpad-specialArgs // { inherit lib; };
              testScript = import ./checks.nix;
              hostPkgs = mkPkgsLinux (import nixpkgs) [ ];
            };
        };
      devShells.x86_64-linux.default = pkgs.haskellPackages.developPackage {
        returnShellEnv = true;
        root = ./haskell;
        withHoogle = false;
        modifier = with pkgs;
          with haskellPackages;
          drv:
          haskell.lib.overrideCabal drv (attrs: {
            buildTools = (attrs.buildTools or [ ])
              ++ [ cabal-install haskell-language-server hlint ];
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
