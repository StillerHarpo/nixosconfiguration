{
  inputs,
  config,
  lib,
  ...
}:
let
  cfg = config.microvm.work;
  ipPrefix = "172.16.100";
  ipRange = "24";
  gatewayIp = "${ipPrefix}.1";
  vmIp = "${ipPrefix}.2";
in
{
  options.microvm = {
    work = {
      enable = lib.mkEnableOption (lib.mdDoc "Enable the work microvm");
      sshKeys = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = lib.mdDoc "ssh keys which are allowed to connect to the work microvm";
      };
      externalInterfaces = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = lib.mdDoc "external interfaces over which the microvm should get access to the internet";
      };
    };
  };

  config = lib.mkIf (cfg.enable == true) {
    networking = {
      networkmanager.unmanaged = [
        "interface-name:br0"
        "interface-name:vm-*"
      ];

      nat = {
        enable = true;
        internalInterfaces = [ "br0" ];
        extraCommands = lib.concatMapStringsSep "\n" (interface: ''
          iptables -t nat -A POSTROUTING -s ${ipPrefix}.0/${ipRange} -o ${interface} -j MASQUERADE
        '') cfg.externalInterfaces;
      };

      firewall.trustedInterfaces = [ "br0" ];
    };

    boot.kernel.sysctl."net.ipv4.ip_forward" = 1;

    systemd.network = {
      enable = true;

      netdevs."br0" = {
        netdevConfig = {
          Name = "br0";
          Kind = "bridge";
        };
      };

      networks = {
        "10-microvm-taps" = {
          matchConfig.Name = "vm-*";
          networkConfig.Bridge = "br0";
        };

        "10-lan-bridge" = {
          matchConfig.Name = "br0";
          networkConfig = {
            Address = [ "${gatewayIp}/${ipRange}" ];
          };
          linkConfig.RequiredForOnline = "no";
        };
      };
    };

    # TODO use constant sshkey
    # programs.ssh.knownHosts.micvmwork = {
    #   hostNames = [ vmIp ];
    #   publicKeyFile = ./backup.pub;
    # };

    microvm.vms = {
      work = {
        autostart = false;
        config =
          { pkgs, config, ... }:
          {
            microvm = {

              mem = 8192;
              balloon = true;
              writableStoreOverlay = "/nix/.rw-store";

              volumes = [
                {
                  image = "nix-store-overlay.img";
                  mountPoint = config.microvm.writableStoreOverlay;
                  size = 20480;
                }
              ];
              registerWithMachined = true;
              shares = [
                {
                  source = "/nix/store";
                  mountPoint = "/nix/.ro-store";
                  tag = "ro-store";
                  proto = "virtiofs";
                }
                {
                  proto = "virtiofs";
                  tag = "meqo";
                  source = "/home/florian/meqo";
                  mountPoint = "/home/work/meqo";
                }
                {
                  proto = "virtiofs";
                  tag = "claude";
                  source = "claude";
                  mountPoint = "/home/work/.claude";
                }
                {
                  source = "/var/lib/microvms/work/ssh-host-keys";
                  mountPoint = "/etc/ssh/persistent-keys";
                  tag = "ssh-keys-share";
                  proto = "virtiofs";
                }
                {
                  source = "claude-state"; # host path
                  mountPoint = "/home/work/.claude-state";
                  tag = "claude-state";
                  proto = "virtiofs"; # virtiofs preferred over 9p for correctness/perf
                }
              ];

              interfaces = [
                {
                  type = "tap";
                  id = "vm-tap0";
                  mac = "02:00:00:00:00:01";
                }
              ];
            };

            services.qemuGuest.enable = true;
            systemd.services."qemu-guest-agent".wantedBy = [ "multi-user.target" ];

            services.openssh = {
              enable = true;
              settings = {
                PermitRootLogin = "yes";
                PasswordAuthentication = false;
              };
              hostKeys = [
                {
                  path = "/etc/ssh/persistent-keys/ssh_host_rsa_key";
                  type = "rsa";
                  bits = 4096;
                }
                {
                  path = "/etc/ssh/persistent-keys/ssh_host_ed25519_key";
                  type = "ed25519";
                }
              ];
            };

            systemd.network = {
              enable = true;
              networks."20-lan" = {
                matchConfig.Type = "ether";
                networkConfig = {
                  Address = [ "${vmIp}/${ipRange}" ];
                  Gateway = gatewayIp;
                  DNS = [ "1.1.1.1" ];
                  DHCP = "no";
                };
              };
            };

            users.users.work = {
              isNormalUser = true;
              hashedPasswordFile = "${pkgs.writeText "password" "$y$j9T$DrA2chw40lirPPr5xy/ka0$p36qBLHR8L0bNGDTwVE4RtAw93QTh2WOvtvW4JrpyR7"}";
              openssh.authorizedKeys.keys = cfg.sshKeys;
            };

            environment.systemPackages =
              let
                pkgs-old = import inputs.nixpkgs-old { system = "x86_64-linux"; };
                pkgs-ghc9102 = import inputs.nixpkgs-ghc9102 { system = "x86_64-linux"; };
                myDevToolsAndLibraries = with pkgs; [ protobuf ];
                stack-wrapped =
                  with pkgs;
                  symlinkJoin {
                    name = "stack";
                    paths = [ stack ];
                    buildInputs = [ makeWrapper ];
                    postBuild = ''
                      wrapProgram $out/bin/stack \
                        --add-flags "\
                          --no-nix \
                          --system-ghc \
                          --no-install-ghc \
                        "\
                        --set LD_LIBRARY_PATH ${pkgs.lib.makeLibraryPath myLibraries}
                    '';
                  };
                postgresql96-wrapped =
                  with pkgs-old;
                  symlinkJoin {
                    name = "postgresql96";
                    paths = [
                      (import "${inputs.nixpkgs-postgres}/pkgs/servers/sql/postgresql" pkgs-old).postgresql_9_6
                    ];
                    buildInputs = [ makeWrapper ];
                    postBuild = ''
                      for exe in $(ls $out/bin/); do
                        wrapProgram $out/bin/''${exe} \
                          --unset LD_LIBRARY_PATH
                      done
                    '';
                  };
                myLibraries =
                  with pkgs;
                  [
                    qrencode
                    snappy
                    icu63
                    gmp
                    libffi
                    postgresql
                    pcre
                    leveldb
                    unixodbc
                    pkg-config
                    cairo
                    pango
                    which
                    libphonenumber
                    glib
                    harfbuzz
                    pcre2
                    util-linux.dev
                    expat.dev
                    libXdmcp.dev
                    libselinux.dev
                    libsepol.dev
                    fribidi.dev
                    libthai.dev
                    libdatrie.dev
                    libsysprof-capture
                  ]
                  ++ myDevToolsAndLibraries;
                myDevTools =
                  with pkgs;
                  [
                    postgresql96-wrapped
                    stack-wrapped
                    pkgs-ghc9102.haskell.compiler.ghc9102
                    pkgs-ghc9102.haskell.packages.ghc9102.hasktags
                    pkgs-ghc9102.haskell.packages.ghc9102.ghcid
                    pkgs-ghc9102.haskell.packages.ghc9102.haskell-language-server
                    pkgs-ghc9102.haskell.packages.ghc9102.HTF
                    hlint
                    openapi-generator-cli
                    tzdata
                    ack
                    autoconf
                    bzip2
                    curl
                    dos2unix
                    fakeroot
                    figlet
                    fio
                    gawk
                    gcc
                    html2text
                    htop
                    imagemagick
                    ioping
                    jq
                    freetype
                    cacert
                    nodejs_22
                    poppler-utils
                    (pkgs.python312.withPackages (ps: [
                      ps.ansible
                      ps.dacite
                      ps.future
                      ps.openpyxl
                      ps.python-ldap
                      ps.requests
                      (ps.buildPythonPackage rec {
                        pname = "hl7apy";
                        version = "1.3.5";
                        src = ps.fetchPypi {
                          inherit pname version;
                          hash = "sha256-TtAq5XTdz5CEs4m/yvHpAjxLL036LOSrQsQwxBgKA/4=";
                        };
                        doCheck = false;
                        pyproject = true;
                        build-system = [ ps.setuptools ];
                      })
                      (ps.buildPythonPackage rec {
                        pname = "ansible-vault";
                        version = "2.1.0";
                        src = ps.fetchPypi {
                          inherit pname version;
                          sha256 = "sha256-XOj9tUcPFEm3a/B64qvFZIDa1INWrkBchbaG77ZNvV4";
                        };
                        doCheck = false;
                        propagatedBuildInputs = [ ps.ansible ];
                        pyproject = true;
                        build-system = [ ps.setuptools ];
                      })
                    ]))
                    python3Packages.supervisor
                    python3Packages.fontforge
                    pgcli
                    pyright
                    libxslt
                    openjdk17
                    maven
                    wget
                    rsync
                    unrtf
                    pkgs-old.envoy
                    chrpath
                    dcmtk
                    iputils
                    pdftk
                    llvmPackages.bintools
                  ]
                  ++ myLibraries;
              in
              myDevTools
              ++ [
                pkgs.claude-code
                pkgs.ripgrep
                pkgs.fd
                pkgs.git
              ];

            system.stateVersion = "25.11";

            # imports = [
            #   inputs.home-manager-flake.nixosModules.home-manager
            #   inputs.agenix.nixosModules.age
            #   outputs.nixosModules
            # ];

            # home-manager.users.work.emacs.enable = true;

            services.emacs = {
              enable = true;
              package = pkgs.emacs-unstable;
            };
            nix = {
              extraOptions = ''
                experimental-features = nix-command flakes
              '';
              settings = {
                substituters = [ "https://claude-code.cachix.org" ];
                trusted-public-keys = [ "claude-code.cachix.org-1:YeXf2aNu7UTX8Vwrze0za1WEDS+4DuI2kVeWEE4fsRk=" ];
              };
            };
            systemd.tmpfiles.rules = [
              "L+ /home/work/.claude.json - - - - /home/work/.claude-state/claude.json"
            ];

            nixpkgs.overlays = [
              inputs.claude-code.overlays.default
              inputs.emacs-overlay.overlay
            ];
          };
      };
    };
  };
}
