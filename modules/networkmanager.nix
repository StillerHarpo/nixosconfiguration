{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.networkmanager;
in
{
  options = {
    networkmanager = {
      enable = lib.mkEnableOption (lib.mdDoc "Wether to use networkmanger");
      # TODO this has to be a list (for usb-c lan)
      lanInterface = lib.mkOption {
        type = lib.types.str;
        description = lib.mdDoc "interface of the lan module. To turn of wifi, if it is up";
      };
    };

  };

  config = lib.mkIf (cfg.enable == true) {
    networking.networkmanager = {
      enable = true;
      dispatcherScripts = [
        {
          source =
            let
              nmcli = "${pkgs.networkmanager}/bin/nmcli";
            in
            pkgs.writeText "wlan_auto_toggle" ''
              if [ "$1" = "${cfg.lanInterface}" ]; then
                  case "$2" in
                      up)
                          ${nmcli} radio wifi off
                          ;;
                      down)
                          ${nmcli} radio wifi on
                          ;;
                      *)
                          if [ "$(${nmcli} -g GENERAL.STATE device show ${cfg.lanInterface})" = "20 (unavailable)" ]; then
                              echo "Lan is $2 (not up/down)"
                              echo "Enabeling wifi"
                              ${nmcli} radio wifi on
                          fi
                          ;;
                  esac
              elif [ "$(${nmcli} -g GENERAL.STATE device show ${cfg.lanInterface})" = "20 (unavailable)" ]; then
                  ${nmcli} radio wifi on
              fi
            '';
          type = "basic";
        }
      ];

    };

    systemd = {
      services =
        let
          targets = [
            "hibernate.target"
            "hybrid-sleep.target"
            "suspend.target"
            "sleep.target"
            "suspend-then-hibernate.target"
          ];
          serviceConfig = {
            Type = "oneshot";
            User = "florian";
            Group = "users";
          };
          environment = {
            DISPLAY = ":0";
          };
        in
        {
          "my-post-resume" = {
            description = "Post-Resume Actions";
            after = targets;
            wantedBy = targets;
            script = ''
              if [ "$(${pkgs.networkmanager}/bin/nmcli -g GENERAL.STATE device show ${cfg.lanInterface})" = "20 (unavailable)" ]; then
                echo "Enabeling wifi"
                ${pkgs.networkmanager}/bin/nmcli radio wifi on
              fi
            '';
            inherit serviceConfig environment;
            enable = true;
          };
        };
    };

  };
}
