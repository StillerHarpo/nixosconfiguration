{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.xserver;
in
{
  options.xserver = {
    enable = mkEnableOption (lib.mdDoc "Wether to use xserver");
    autoLogin = mkEnableOption (lib.mdDoc "Wether to autologin into wayland");
  };
  config = mkIf cfg.enable {

    apparmor.systemPackages =
      [ { pkgs = with pkgs; [
            scrot
            slack
            arandr
            xsel
            xsecurelock
            dzen2
            xclip
          ];
          rules = lib.apparmor.defaultRules;
        }
      ];

    services = {

      xserver = {
        enable = true;
        xkbOptions = "eurosign:e, caps:escape, grp:alt_shift_toggle";
        xkbVariant = "altgr-intl";
        monitorSection = ''Option "DPMS" "false"'';
        serverLayoutSection = ''
        Option          "BlankTime"     "0"
        Option          "StandbyTime"   "0"
        Option          "SuspendTime"   "0"
        Option          "OffTime"       "0"
      '';
        # Enable XMonad
        windowManager = {
          xmonad = {
            enable = true;
            enableContribAndExtras = true;
          };
        };
        displayManager = mkIf cfg.autoLogin {
          defaultSession = "none+xmonad";
          autoLogin = {
            enable = true;
            user = "florian";
          };
        };
        resolutions = [
          {
            x = 2560;
            y = 1440;
          }
          {
            x = 1920;
            y = 1080;
          }
        ];
        windowManager.xmonad.extraPackages = haskellPackages:
          with haskellPackages; [
            MissingH
            protolude
          ];
        xautolock = {
          enable = true;
          locker = "${pkgs.xsecurelock}/bin/xsecurelock";
          killtime = 200;
          killer = "/run/current-system/systemd/bin/systemctl hibernate";
        };
      };
      picom = {
        enable = true;
        inactiveOpacity = 0.8;
        opacityRules = [
          "100:name = 'Dmenu'"
          "100:name = 'Rofi'"
          "100:class_g ?= 'Rofi'"
          "100:name = 'Notification'"
        ];
      };
      redshift = {
        enable = true;
        brightness = {
          day = "0.8";
          night = "0.7";
        };
        temperature.night = 1501;
      };
      unclutter-xfixes.enable = true;

      autorandr = {
        enable = true;
        profiles = {
          default = {
            fingerprint.eDP-1 =
              "00ffffffffffff0006af362300000000001b0104a51f117802f4f5a4544d9c270f505400000001010101010101010101010101010101e65f00a0a0a040503020350035ae100000180000000f0000000000000000000000000020000000fe0041554f0a202020202020202020000000fe004231343051414e30322e33200a00b2";
            config = {
              eDP-1 = {
                crtc = 0;
                mode = "2560x1440";
                position = "0x0";
                rate = "60.01";
              };
              DP-1.enable = false;
              HDMI-1.enable = false;
              DP-2.enable = false;
              HDMI-2.enable = false;
              DP-2-1.enable = false;
              DP-2-2.enable = false;
              DP-2-3.enable = false;
            };
          };
          home = {
            fingerprint = {
              eDP-1 =
                "00ffffffffffff0006af362300000000001b0104a51f117802f4f5a4544d9c270f505400000001010101010101010101010101010101e65f00a0a0a040503020350035ae100000180000000f0000000000000000000000000020000000fe0041554f0a202020202020202020000000fe004231343051414e30322e33200a00b2";
              HDMI-2 =
                "00ffffffffffff0009d1a77845540000181a010380351e782eba45a159559d280d5054a56b80810081c08180a9c0b300d1c001010101023a801871382d40582c4500132a2100001e000000ff0047364730353537363031390a20000000fd00324c1e5311000a202020202020000000fc0042656e5120474c32343530480a0146020322f14f90050403020111121314060715161f2309070765030c00100083010000023a801871382d40582c4500132a2100001f011d8018711c1620582c2500132a2100009f011d007251d01e206e285500132a2100001e8c0ad08a20e02d10103e9600132a21000018000000000000000000000000000000000000000000eb";
            };
            config = {
              eDP-1 = {
                crtc = 1;
                mode = "2560x1440";
                position = "0x1080";
                rate = "60.01";
              };
              HDMI-1.enable = false;
              HDMI-2 = {
                primary = true;
                crtc = 0;
                mode = "1920x1080";
                position = "0x0";
                rate = "60.00";
              };
              DP-2.enable = false;
              DP-2-1.enable = false;
              DP-2-2.enable = false;
              DP-2-3.enable = false;
            };
          };
          home2 = {
            fingerprint.DP-2 = "00ffffffffffff0009d1a77845540000181a010380351e782eba45a159559d280d5054a56b80810081c08180a9c0b300d1c001010101023a801871382d40582c4500132a2100001e000000ff0047364730353537363031390a20000000fd00324c1e5311000a202020202020000000fc0042656e5120474c32343530480a0146020322f14f90050403020111121314060715161f2309070765030c00100083010000023a801871382d40582c4500132a2100001f011d8018711c1620582c2500132a2100009f011d007251d01e206e285500132a2100001e8c0ad08a20e02d10103e9600132a21000018000000000000000000000000000000000000000000eb";
            fingerprint.eDP-1 = "00ffffffffffff0006af362300000000001b0104a51f117802f4f5a4544d9c270f505400000001010101010101010101010101010101e65f00a0a0a040503020350035ae100000180000000f0000000000000000000000000020000000fe0041554f0a202020202020202020000000fe004231343051414e30322e33200a00b2";
            config = {
              eDP-1 = {
                crtc = 0;
                mode = "2560x1440";
                position = "0x1440";
                rate = "60.01";
              };
              HDMI-1.enable = false;
              HDMI-2.enable = false;
              DP-1.enable = false;
              DP-2 = {
                primary = true;
                crtc = 0;
                mode = "1920x1080";
                position = "0x0";
                rate = "60.00";
              };
              DP-2-1.enable = false;
              DP-2-2.enable = false;
              DP-2-3.enable = false;
            };
          };
          "work" = {
            fingerprint = {
              eDP1 =
                "00ffffffffffff0006af362300000000001b0104a51f117802f4f5a4544d9c270f505400000001010101010101010101010101010101e65f00a0a0a040503020350035ae100000180000000f0000000000000000000000000020000000fe0041554f0a202020202020202020000000fe004231343051414e30322e33200a00b2";
              DP-2-1 =
                "00ffffffffffff0010ac80405333323732170104a53c22783a4bb5a7564ba3250a5054a54b008100b300d100714fa940818001010101565e00a0a0a029503020350055502100001a000000ff00374a4e5935334342373233530a000000fc0044454c4c205532373133484d0a000000fd0031561d711e010a20202020202001be02031df15090050403020716010611121513141f2023097f0783010000023a801871382d40582c250055502100001e011d8018711c1620582c250055502100009e011d007251d01e206e28550055502100001e8c0ad08a20e02d10103e960055502100001800000000000000000000000000000000000000000000000000005d";
            };
            config = {
              eDP-1 = {
                crtc = 0;
                mode = "2560x1440";
                position = "0x1440";
                rate = "60.01";
              };
              HDMI-1.enable = false;
              HDMI-2.enable = false;
              DP-1.enable = false;
              DP-2.enable = false;
              DP-2-1 = {
                primary = true;
                crtc = 1;
                mode = "2560x1440";
                position = "0x0";
                rate = "59.9";
              };
              DP-2-2.enable = false;
              DP-2-3.enable = false;
            };

          };
          "work2" = {
            fingerprint = {
              eDP1 =
                "00ffffffffffff0006af362300000000001b0104a51f117802f4f5a4544d9c270f505400000001010101010101010101010101010101e65f00a0a0a040503020350035ae100000180000000f0000000000000000000000000020000000fe0041554f0a202020202020202020000000fe004231343051414e30322e33200a00b2";
              DP-1-1 =
                "00ffffffffffff0010ac80405333323732170104a53c22783a4bb5a7564ba3250a5054a54b008100b300d100714fa940818001010101565e00a0a0a029503020350055502100001a000000ff00374a4e5935334342373233530a000000fc0044454c4c205532373133484d0a000000fd0031561d711e010a20202020202001be02031df15090050403020716010611121513141f2023097f0783010000023a801871382d40582c250055502100001e011d8018711c1620582c250055502100009e011d007251d01e206e28550055502100001e8c0ad08a20e02d10103e960055502100001800000000000000000000000000000000000000000000000000005d";
            };
            config = {
              eDP-1 = {
                crtc = 0;
                mode = "2560x1440";
                position = "0x1440";
                rate = "60.01";
              };
              HDMI-1.enable = false;
              HDMI-2.enable = false;
              DP-1.enable = false;
              DP-2.enable = false;
              DP-1-1 = {
                primary = true;
                crtc = 1;
                mode = "2560x1440";
                position = "0x0";
                rate = "59.9";
              };
              DP-2-2.enable = false;
              DP-2-3.enable = false;
            };
          };
        };

      };
    };
  };
}
