{
  inputs,
  outputs,
  config,
  lib,
  pkgs,
  private,
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
      mem = lib.mkOption {
        type = lib.types.number;
        description = lib.mdDoc "available memory for the vm";
      };
      vcpu = lib.mkOption {
        type = lib.types.number;
        description = lib.mdDoc "available virtual cpus for the vm";
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

    microvm.vms = {
      work =
        let
          pkgs = import inputs.nixpkgs-unstable {
            system = "x86_64-linux";
            config.allowUnfree = true;
            overlays = [
              (final: prev: { inherit private; })
              inputs.claude-code.overlays.default
            ]
            ++ builtins.attrValues outputs.overlays;
          };
        in
        {
          autostart = false;
          config =
            { config, ... }:

            let
              myDevToolsAndLibraries = with pkgs; [
                protobuf
              ];
              myLibraries =
                with pkgs;
                [
                  libpng
                  qrencode
                  snappy
                  icu63
                  gmp
                  libffi
                  postgresql.lib
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
                  util-linux
                  expat
                  libxdmcp
                  libselinux
                  libsepol
                  fribidi
                  libthai
                  libdatrie
                  libsysprof-capture
                  zlib
                ]
                ++ myDevToolsAndLibraries;
            in
            {
              microvm = {
                vmHostPackages = pkgs;
                mem = cfg.mem;
                vcpu = cfg.vcpu;
                balloon = true;
                writableStoreOverlay = "/nix/.rw-store";

                volumes = [
                  {
                    image = "nix-store-overlay.img";
                    mountPoint = config.microvm.writableStoreOverlay;
                    size = 20480;
                  }
                  {
                    image = "node-modules.img";
                    mountPoint = "/home/work/meqo/meqo/server/js/node_modules";
                    size = 4096;
                    mkfsExtraArgs = [
                      "-E"
                      "root_owner=1000:100"
                    ];
                  }
                  {
                    mountPoint = "/home/work/.cache";
                    image = "cache.img";
                    size = 4096;
                    mkfsExtraArgs = [
                      "-E"
                      "root_owner=1000:100"
                    ];
                  }
                  {
                    mountPoint = "/home/work/.DociSSL";
                    image = "docissl.img";
                    size = 1024;
                    mkfsExtraArgs = [
                      "-E"
                      "root_owner=1000:100"
                    ];
                  }
                  {
                    mountPoint = "/home/work/meqo/meqo/server/demo/node_modules";
                    image = "nodemodules.img";
                    size = 128;
                    mkfsExtraArgs = [
                      "-E"
                      "root_owner=1000:100"
                    ];
                  }
                  {
                    mountPoint = "/home/work/.directory_history";
                    image = "directory_history.img";
                    size = 1024;
                    mkfsExtraArgs = [
                      "-E"
                      "root_owner=1000:100"
                    ];
                  }
                  {
                    mountPoint = "/home/work/Dokumente";
                    image = "dokumente.img";
                    size = 1024;
                    mkfsExtraArgs = [
                      "-E"
                      "root_owner=1000:100"
                    ];
                  }
                  {
                    image = "DociSSL-state.img";
                    mountPoint = "/home/work/.DociSSL-state";
                    size = 1024;
                    mkfsExtraArgs = [
                      "-E"
                      "root_owner=1000:100"
                    ];
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
                  # TODO move everything below to volumes
                  {
                    proto = "virtiofs";
                    tag = "claude";
                    source = "claude";
                    mountPoint = "/home/work/.claude";
                  }
                  # TODO can be remvoved and replaced with socat
                  {
                    source = "/var/lib/microvms/work/ssh-host-keys";
                    mountPoint = "/etc/ssh/persistent-keys";
                    tag = "ssh-keys-share";
                    proto = "virtiofs";
                  }
                  {
                    source = "claude-state";
                    mountPoint = "/home/work/.claude-state";
                    tag = "claude-state";
                    proto = "virtiofs";
                  }
                  # 8GB
                  {
                    source = "stack";
                    mountPoint = "/home/work/.stack";
                    tag = "stack";
                    proto = "virtiofs";
                  }
                  {
                    proto = "virtiofs";
                    tag = "DociGateway";
                    source = "/home/florian/.DociGateway-Cache";
                    mountPoint = "/home/work/.DociGateway-Cache";
                  }
                  {
                    proto = "virtiofs";
                    tag = "checkpad";
                    source = "/home/florian/.checkpad";
                    mountPoint = "/home/work/.checkpad";
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

              imports = [
                outputs.nixosModules
                inputs.agenix.nixosModules.age
                inputs.home-manager-flake.nixosModules.home-manager
              ];

              # we are inside a vm and the host already has a firewall
              networking.firewall.enable = false;

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
                extraGroups = [ "wheel" ];
                shell = pkgs.zsh;
              };
              environment = {
                systemPackages =
                  let
                    pkgs-old = import inputs.nixpkgs-old { system = "x86_64-linux"; };
                    pkgs-ghc9102 = import inputs.nixpkgs-ghc9102 { system = "x86_64-linux"; };
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
                            "
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
                  in
                  with pkgs;
                  [
                    gnupg
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
                    pkgs.claude-code
                    pkgs.ripgrep
                    pkgs.fd
                    pkgs.git
                    libreoffice
                  ]
                  ++ myDevToolsAndLibraries;

                pathsToLink = [ "/share/postgresql" ];
              };

              programs.zsh.enable = true;

              # Select internationalisation properties.
              console.keyMap = "us";
              # Select internationalisation properties.
              i18n = {
                defaultLocale = "en_US.UTF-8";
                supportedLocales = [
                  "de_DE.UTF-8/UTF-8"
                  "en_US.UTF-8/UTF-8"
                ];
              };

              # Set your time zone.
              time.timeZone = "Europe/Berlin";

              environment.variables =
                let
                  withDev = pkg: [ pkg ] ++ lib.optional (pkg ? dev) pkg.dev;
                  libs = lib.closePropagation (lib.concatMap withDev myLibraries);
                in
                {
                  PKG_CONFIG_PATH = lib.concatStringsSep ":" [
                    (lib.makeSearchPath "lib/pkgconfig" libs)
                    (lib.makeSearchPath "share/pkgconfig" libs)
                  ];
                  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath libs;
                  LIBRARY_PATH = pkgs.lib.makeLibraryPath libs;
                  C_INCLUDE_PATH = lib.makeSearchPath "include" libs;
                  CPLUS_INCLUDE_PATH = lib.makeSearchPath "include" libs;
                  IN_NIX_SHELL = "microvm";
                  PUPPETEER_SKIP_CHROMIUM_DOWNLOAD = 1;
                  PUPPETEER_EXECUTABLE_PATH = "${pkgs.chromium}/bin/chromium";
                  CPM_TIMEZONEFILE = "${pkgs.tzdata}/share/zoneinfo/Europe/Berlin";
                  LOCALE_ARCHIVE_2_27 = "${pkgs.glibcLocales}/lib/locale/locale-archive";
                  way = "f";
                  CPM_COMPILE_CACHE_SPEC = "local";
                  JAVA_HOME = pkgs.openjdk8;
                };

              system.stateVersion = "26.05";

              nix = {
                extraOptions = ''
                  experimental-features = nix-command flakes
                '';
                settings = {
                  substituters = [ "https://claude-code.cachix.org" ];
                  trusted-public-keys = [ "claude-code.cachix.org-1:YeXf2aNu7UTX8Vwrze0za1WEDS+4DuI2kVeWEE4fsRk=" ];
                };
              };

              nixpkgs = {
                overlays = [
                  (final: prev: { inherit private; })
                ]
                ++ builtins.attrValues outputs.overlays;
              };
              systemd.tmpfiles.rules = [
                "L+ /home/work/.claude.json - - - - /home/work/.claude-state/claude.json"
                "L+ /home/work/DociSSL.tar.xz.gpg  - - - - /home/work/.DociSSL-state/DociSSL.tar.xz.gpg"
              ];

              programs.bash.loginShellInit = ''
                export PATH="/home/work/meqo/meqo/server/js/node_modules/.bin:$PATH"
              '';
              home-manager = {
                useGlobalPkgs = true;
                users.work = { config, ... }: {
                  imports = [
                    outputs.homeModules
                  ];
                  home.stateVersion = "26.05";
                  programs.pgcli = {
                    enable = true;
                    settings.main.keyring = false;
                  };
                  zsh.enable = true;
                  emacs = {
                    enable = true;
                    isWork = true;
                  };
                };
              };

            };
        };
    };
  };
}
