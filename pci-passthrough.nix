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
  environment.systemPackages = with pkgs; [
    virtmanager
    qemu
    OVMF
  ];
  
  virtualisation.libvirtd.enable = true;
  
  users.groups.libvirtd.members = [ "root" "florian"];
  
  virtualisation.libvirtd.qemuVerbatimConfig = ''
    nvram = [
      "${pkgs.OVMF}/FV/OVMF.fd:${pkgs.OVMF}/FV/OVMF_VARS.fd"
    ]
  '';
  networking.firewall.trustedInterfaces = [ "virbr1" ];
}
