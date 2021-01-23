# here are every configs that are used on my laptop but not on my workstation

{ config, pkgs, ... }:

{
  imports = [ ./hibernate.nix ];

  # luks encryption
  boot.initrd.luks.devices.luksroot.device = "/dev/disk/by-uuid/6d8ca465-1ff7-45a5-88d3-9aa0b4807cb7";

  # powerManagement.enable = false;
  services = {
    # Go in hibernate at lid
    logind = {
      lidSwitch = "hibernate";
      extraConfig = ''HandlePowerKey=hibernate
                      RuntimeDirectorySize=30%'';
    };
    # mouse pad
    xserver = {
      dpi = 180;
      synaptics = {
        enable = true;
        twoFingerScroll = true;
      };
    };
  };
  # Bluetooth sound
  hardware.bluetooth.enable = true;

  # wifi
  # networking.wireless.enable = true;
  # boot.initrd.network.enable = true;
  # system.activationScripts.wpa_supplicant=
  #  "ln -sfn /etc/nixos/wpa_supplicant.conf /etc/wpa_supplicant.conf";

  # big font for high resolution
  console.font = "sun12x22";

  programs.light.enable = true;

  systemd = {
    targets.my-post-resume = {
      description = "Post-Resume Actions";
      requires = [ "my-post-resume.service" ];
      after = [ "my-post-resume.service" ];
      wantedBy = [ "sleep.target" ];
      unitConfig.StopWhenUnneeded = true;
    };
    services = {
      # Service executed before suspending/hibernating.
      "my-pre-sleep" = {
        description = "Pre-Sleep Actions";
        wantedBy = [ "sleep.target" ];
        before = [ "sleep.target" ];
        script =
          ''
            ${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ true
          '';
        serviceConfig = {
          Type = "oneshot";
          User = "florian";
          Group = "users";
        };
      };
      "my-post-resume" = {
        description = "Post-Resume Actions";
        after = [ "suspend.target" "hibernate.target" "hybrid-sleep.target" ];
        environment.DISPLAY = ":0";
        script =
          ''
            if ${pkgs.xorg.xrandr}/bin/xrandr -q | grep HDMI-2 | grep -v disconnected
            then
              ${pkgs.xorg.xrandr}/bin/xrandr --output eDP-1 --mode 1920x1080
              ${pkgs.xorg.xrandr}/bin/xrandr --output HDMI-2 --mode 1920x1080 --left-of eDP-1
            else
              ${pkgs.xorg.xrandr}/bin/xrandr --output HDMI-2 --off
              ${pkgs.xorg.xrandr}/bin/xrandr --output --eDP-1 --mode 1920x1080
            fi
            ${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ true
          '';
        serviceConfig = {
          Type = "oneshot";
          User = "florian";
          Group = "users";
        };
      };
    };
  };
}
