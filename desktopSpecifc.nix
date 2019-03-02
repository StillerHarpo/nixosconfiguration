{ config, pkgs, ... }:

{
  imports = [ ./pci-passthrough.nix ];
  environment = {
    systemPackages = with pkgs; [
      lm_sensors
      hdparm
    ];
  };

  boot = {
    # luks encryption
    initrd.luks.devices.luksroot.device = "/dev/sdd2";
    kernelModules = [ "it87" ];
    # preventing kernel error message
    blacklistedKernelModules = [ "sp5100_tco" ];
  };

  # i don't use bluetooth(preventing kernel error message)
  hardware.pulseaudio.configFile = ./default.pa;

  systemd.services= {
    fancontrol = {
      description = "fancontrol daemon";
      wantedBy = [ "multi-user.target" ];
      script = "${pkgs.lm_sensors}/sbin/fancontrol /etc/nixos/fancontrol";
    };

    # stop unused hdds
    hdparm = {
      description = "stop unused hdds";
      after = [ "hibernate.target" "suspend.target" ];
      partOf = [ "hibernate.target" "suspend.target" ];
      wantedBy = [ "multi-user.target" "suspend.target" "hibernate.target" ];
      script = "${pkgs.hdparm}/sbin/hdparm -Y /dev/sda /dev/sdc";
    };
  };

  # Supposedly better for the SSD.
  fileSystems."/".options = [ "noatime" "nodiratime" ];

  i18n.consoleFont = "Lat2-Terminus16";
}
