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
      url = "github:nix-community/emacs-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
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
    disko = {
      url = github:nix-community/disko;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-anywhere = {
      url = github:numtide/nixos-anywhere;
      inputs = {
        nixpkgs.follows = "nixpkgs";
        disko.follows = "disko";
        nixos-2305.follows = "nixpkgs";
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
  outputs = inputs@{ self, ... }:

    let
      inherit (self) outputs;
      lib =
        (inputs.nixpkgs.lib.extend (_: _: inputs.home-manager-flake.lib)).extend
        (import ./mylib);

      thinkpad-modules = [
        inputs.nixpkgs.nixosModules.notDetected
        inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t480s
        inputs.home-manager-flake.nixosModules.home-manager
        {
          home-manager = {
            users.florian = { pkgs, config, ... }: {
              xdg = {
                enable = true;
                configFile."nix/inputs/nixpkgs".source = inputs.nixpkgs.outPath;
              };
              home.sessionVariables.NIX_PATH =
                "nixpkgs=${config.xdg.configHome}/nix/inputs/nixpkgs\${NIX_PATH:+:$NIX_PATH}";
              # Das enthält nicht die overlays
              nix.registry.nixpkgs.flake = inputs.nixpkgs;
            };
            extraSpecialArgs = {
              inherit inputs outputs;
              lib = lib.extend (_: _: inputs.home-manager-flake.lib);
              private = import ./private.nix;
            };
          };
        }
        ./linux/thinkpad/configuration.nix
        inputs.agenix.nixosModules.age
        inputs.envfs.nixosModules.envfs
      ];

      thinkpad-specialArgs = {
        inherit inputs outputs;
        home-manager-flake = inputs.home-manager-flake.nixosModule;
        private = import ./private.nix;
      };
    in {

      overlays = import ./overlays.nix { inherit inputs outputs; };
      nixosConfigurations = {
        nixosThinkpad = inputs.nixpkgs.lib.nixosSystem {
          inherit lib;
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

        desktop = inputs.nixpkgs.lib.nixosSystem {
          specialArgs = {
            inherit inputs outputs;
            private = import ./private.nix;
          };
          modules = with inputs.nixos-hardware.nixosModules; [
            inputs.nixpkgs.nixosModules.notDetected
            common-pc
            common-pc-hdd
            common-pc-ssd
            common-cpu-amd
            common-gpu-amd-southern-islands
            inputs.home-manager-flake.nixosModules.home-manager
            ./linux/desktop/configuration.nix
            inputs.agenix.nixosModules.age
          ];
        };

        nixosRpi4 =  inputs.nixpkgs.lib.nixosSystem {
          specialArgs = {
            inherit inputs outputs;
            private = import ./private.nix;
          };
          system = "aarch64-linux";
          modules = [
            inputs.agenix.nixosModules.age
            inputs.disko.nixosModules.disko
            ./linux/rpi4/configuration.nix
          ];
        };
        
        rpi2 = inputs.nixpkgs.lib.nixosSystem {
          system = "armv7l-linux";

          modules = [
            "${inputs.nixpkgs}/nixos/modules/installer/sd-card/sd-image-armv7l-multiplatform.nix"
            {
              users.users = {
                root.openssh.authorizedKeys.keys = [
                  "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDIc5ug7o0Jqe6SNNrjW5BAyTWqZGVvehJ7GOZrT7DFiiop174CdDlRo4GZlvAGzFEaWr5/4knS0p8kErDPgcgdfC0IL2BYClPEna8agHhyqvmZISxfFDk48Bg/yGo37iPpuGxT7g6VIqI46PnTqgF3nfX1J3crPDD1tDUv5Nq+LH3qlwnpRA3rMBTym/QPkPAM8jQGB4DtyhI1s6UBEQK5vvljhYBG/P54ILQUokYqIsUirQKpBW7Z3sY+zezJpOc+Y6DRZ0rm9dRa6HsOFQ1DQ6u3FkBcyq+vkr4KWmxDdRO0acAV6o0c+1dqyhdaKfklO1E9ZOScTG9Wur9p17qMPsd1zJ8OZ5S7NDMMFi2wcUkxQO9QNqndo+opOBYVMxrz2Hc2Ch2vzuSlVwUxKE60qfARFZ5ZbVOJ8Ate+vghrldgyRF7Sg5yid8Rv6RHv4nvJZpEFjmtkluWzNwhoaF9ifNdB2y7MZeDgu1n1v9xmexSYs97cCB+sprSLYrqgsk= florian@nixos-thinkpad"
                  "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCg3kPf2YxtGrfckExx/ckZrsXH1sa0mDGAAVaK6VOpLF4s0fnWJmXrBSfeGgOVPHRCNF3dfQR34u5PwO3gtK8FQr8XvVRKP+EcMi3ztKHcawnThJyB2Zi3D8yTGDYnKRXoiDvitTWtDDmZta91QJZsK65R0SeOGSk1lG6MOYyEhTRQS4rV1Ij8qEqAMB7R7/yUIkdoyGDbcDZ05Bs8/NCfBLJ/pv+pRKp+ZFIjqmKNVYznZ4OT7ywbbQaTNUCoQO4Hcm+ujlOw2jV7Xqb9842uBRQKNKik3hGw1DlAxKrbK/s3Uu0Vs/cUrEoH68+tKZdCzoX3YoYI9cyMRS2+LEkb florian@florian"
                  "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDJjjBxd9De7YH9ZCzSLmmBTXYmMOdPaLuToZc5zFe/FUz8V8NhQCmHnPP/wcL2/3gz8jz19PcXKN4jc204Nx+XrFvXPO2BMuaeztwyhLd7o5LLmWEm66PImXq4BPLUH/QTlSumIDPwnehUnorUVEcso2VSBidZ4hor2FTIvP3x3DT19xw+cH19fh6xjSEx3bYk8aQTbXpxQAduou8Q424fmOuaQcCO9f3odYmuhVhmN6hjtCK/NhcKQRQ25C1Ftw+NcC++e2c196J4VLTB3XybKU4BYPI9A1Lq8QHNS16JapGWcvoaamx5o8Br+zxmmBlTJvhMSEUga02QXMrVPkTaDDAwtzSBl1mSD9lx5haCySCH9vsjfR5aoc5Axm1qPiO+kxlLSCmdd7CxexWPR0WbmCt7toPHXGoeVHxY2p6AdeZDdgdr1ccf2iLcE7Y37cwvEBiQx0KCAN3kkYAdSBxFStUDUMtb+cbrnDAqCztg5LIr4GoT5PeXtPW2CCUl74c= root@nixos-thinkpad"
                ];
              };
              system.stateVersion = "22.11";
              boot.supportedFilesystems =
                inputs.nixpkgs.legacyPackages."x86_64-linux".lib.mkForce [
                  "btrfs"
                  "reiserfs"
                  "vfat"
                  "f2fs"
                  "xfs"
                  "ntfs"
                  "cifs"
                ];
              nixpkgs = {
                pkgs =
                  inputs.nixpkgs.legacyPackages."x86_64-linux".pkgsCross.armv7l-hf-multiplatform;
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

      deploy.nodes = {
        desktop = {
          hostname = "192.168.178.24";
          profiles.system = {
            user = "root";
            sshUser = "root";
            path = inputs.deploy-rs.lib.x86_64-linux.activate.nixos
              self.nixosConfigurations.desktop;
          };
        };
        rpi4 = {
          hostname = (import ./variables.nix).nixosRpi4IP;
          profiles.system = {
            user = "root";
            sshUser = "root";
            remoteBuild = true;
            path = inputs.deploy-rs.lib.aarch64-linux.activate.nixos
              self.nixosConfigurations.nixosRpi4;
          };
        };
      };

      # This is highly advised, and will prevent many possible mistakes
      checks = lib.recursiveUpdate (builtins.mapAttrs
        (system: deployLib: deployLib.deployChecks self.deploy)
        inputs.deploy-rs.lib) {
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
                systemd.services.backblaze = lib.mkForce { };
                networking.wg-quick.interfaces = lib.mkForce { };
              };
              node.specialArgs = thinkpad-specialArgs // {
                inherit lib;
                pkgs = outputs.packages."x86_64-linux";
              };
              testScript = import ./checks.nix;
              hostPkgs = outputs.packages."x86_64-linux";
            };
        };
      darwinConfigurations."Florians-MBP" = inputs.darwin.lib.darwinSystem {
        modules = [
          ./mac/configuration.nix
          inputs.home-manager-flake.darwinModules.home-manager
        ];
      };
    } // inputs.flake-utils.lib.eachDefaultSystem (system: {
      packages = import inputs.nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [ (_: _: { inherit system; }) ]
          ++ (builtins.attrValues outputs.overlays);
      };
      legacyPackages = outputs.packages."${system}";
      devShells.default =
        outputs.packages."${system}".haskellPackages.developPackage {
          returnShellEnv = true;
          root = ./haskell;
          withHoogle = false;
          modifier = with outputs.packages."${system}";
            with haskellPackages;
            drv:
            haskell.lib.overrideCabal drv (attrs: {
              buildTools = (attrs.buildTools or [ ])
                ++ [ cabal-install haskell-language-server hlint ];
            });
        };

    });
}
