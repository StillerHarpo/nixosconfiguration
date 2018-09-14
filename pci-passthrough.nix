# put this file in /etc/nixos/
# change the settings tagged with "CHANGE:"
# and add
#   ./pci-passthrough.nix
# to /etc/nixos/configuration.nix in `imports`

{config, pkgs, ... }:
{
  boot = {
    # CHANGE: intel_iommu enables iommu for intel CPUs with VT-d
    # use amd_iommu if you have an AMD CPU with AMD-Vi
    kernelParams = [ "amd_iommu=on" ];

    # These modules are required for PCI passthrough, and must come before early modesetting stuff
    kernelModules = [ "vfio" "vfio_iommu_type1" "vfio_pci" "vfio_virqfd" ];

    # CHANGE: Don't forget to put your own PCI IDs here
    extraModprobeConfig ="options vfio-pci ids=1002:6798,1002:aaa0";
  };

  # synchronize audio sample rate with windows
  hardware.pulseaudio.daemon.config = {
    default-sample-rate = 44100;
    alternate-sample-rate = 48000;
  };

  environment.systemPackages = with pkgs; [
    virtmanager
    qemu
    OVMF
    looking-glass-client
  ];

  systemd.services.sharedMem = {
    description = "shared memory for looking glass";
    wantedBy = [ "multi-user.target" ];
    script = ''
      touch /dev/shm/looking-glass
      chown polkituser:libvirtd /dev/shm/looking-glass
      chmod 660 /dev/shm/looking-glass
    '';
  };

  virtualisation.libvirtd.enable = true;

  users.groups.libvirtd.members = [ "root" "florian"];

  virtualisation.libvirtd.qemuVerbatimConfig = ''
    nvram = [
      "${pkgs.OVMF}/FV/OVMF.fd:${pkgs.OVMF}/FV/OVMF_VARS.fd"
    ]
    user = "florian"
  '';
  networking.firewall.trustedInterfaces = [ "virbr1" ];
}
