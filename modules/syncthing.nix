{
  config,
  lib,
  ...
}:

let
  cfg = config.syncthing;

in
{
  options = {
    syncthing = {
      enable = lib.mkEnableOption (lib.mdDoc "Wether to use syncthing");
      otherLaptopId = lib.mkOption {
        type = lib.types.str;
        description = lib.mdDoc "the syncthing id of the other laptop";
      };
      cert = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = ''
          Path to the `cert.pem` file, which will be copied into Syncthing's
          [configDir](#opt-services.syncthing.configDir).
        '';
      };

      key = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = ''
          Path to the `key.pem` file, which will be copied into Syncthing's
          [configDir](#opt-services.syncthing.configDir).
        '';
      };
    };

  };

  config = lib.mkIf (cfg.enable == true) {
    services.syncthing = {
      enable = true;
      key = cfg.key;
      cert = cfg.cert;
      user = "florian";
      dataDir = "/home/florian/.syncthing";
      settings = {
        devices = {
          "android".id = "VWFGCVO-56ZMY6L-5N7MQ5F-GB4TJFS-AHAGT5L-WYN4WTS-TQJHEVN-NBBOOAS";
          "macos".id = "SEEGNGR-RV3PPXZ-AYPLTV3-VAEUOAO-IACRN32-Z4IEBCO-NECN453-FUF6OA3";
          "pi".id = "D5UJHLR-NVAXNBN-7HD6XXP-7IOAKHA-6VT4B2Y-IBYPHKU-2KFFDPW-PUSX4QV";
          "otherLaptop".id = cfg.otherLaptopId;
        };
        folders = {
          "android-photos" = {
            path = "/home/florian/android/photos";
            devices = [ "android" ];
          };
          "android-pictures" = {
            path = "/home/florian/android/pictures";
            devices = [ "android" ];
          };
          "android-org" = {
            path = "/home/florian/android/org";
            devices = [ "android" ];
          };
          "android-backups" = {
            path = "/home/florian/android/backups";
            devices = [
              "android"
              "pi"
            ];
          };
          "android-share" = {
            path = "/home/florian/android/share";
            devices = [ "android" ];
          };
          "org-roam" = {
            path = "/home/florian/Dokumente/org-roam";
            devices = [
              "android"
              "pi"
            ];
            versioning = {
              type = "simple";
              params.keep = "5";
            };
          };
          "music" = {
            path = "/home/florian/Music";
            devices = [ "android" ];
          };
          "macos" = {
            path = "/home/florian/syncthing-macos";
            devices = [ "macos" ];
          };
          "documents" = {
            path = "/home/florian/Dokumente";
            devices = [ "otherLaptop" ];
          };
        };
      };
    };
  };

}
