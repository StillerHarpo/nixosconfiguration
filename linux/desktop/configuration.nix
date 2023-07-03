{ config, home-manager, pkgs, ... }:

{

  imports = [ ../configuration.nix ];

  boot.loader = {
    systemd-boot.enable = false;
    efi = {
      canTouchEfiVariables = true;
      efiSysMountPoint = "/boot";
    };
    grub = {
      device = "nodev";
      enable = true;
      efiSupport = true;
      version = 2;
      useOSProber = true;
    };
  };

  boot = {
    initrd = {
      availableKernelModules = [
        "ahci"
        "ohci_pci"
        "ehci_pci"
        "xhci_pci"
        "usbhid"
        "usb_storage"
        "sd_mod"
        "sr_mod"
      ];
    };
    kernelModules = [ "kvm-amd" ];
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/550f9ea8-74b0-4775-ac06-f178089dc9a4";
      fsType = "ext4";
    };
    "/boot" = {
      device = "/dev/disk/by-uuid/EDCD-D594";
      fsType = "vfat";
    };
  };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/db9038db-4602-4d7e-9a4f-5d56f43c8db5"; }];

  networking = {
    useDHCP = false;
    interfaces.enp10s0.useDHCP = true;
    firewall.allowedTCPPorts = [ 22 ];
  };

  # Enable SSH in the boot process.
  services = {
    openssh = {
      enable = true;
      permitRootLogin = "yes";
      passwordAuthentication = false;
    };
  };

  home-manager.users.florian = import ./home/configuration.nix;

  users.users = {
    root.openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDIc5ug7o0Jqe6SNNrjW5BAyTWqZGVvehJ7GOZrT7DFiiop174CdDlRo4GZlvAGzFEaWr5/4knS0p8kErDPgcgdfC0IL2BYClPEna8agHhyqvmZISxfFDk48Bg/yGo37iPpuGxT7g6VIqI46PnTqgF3nfX1J3crPDD1tDUv5Nq+LH3qlwnpRA3rMBTym/QPkPAM8jQGB4DtyhI1s6UBEQK5vvljhYBG/P54ILQUokYqIsUirQKpBW7Z3sY+zezJpOc+Y6DRZ0rm9dRa6HsOFQ1DQ6u3FkBcyq+vkr4KWmxDdRO0acAV6o0c+1dqyhdaKfklO1E9ZOScTG9Wur9p17qMPsd1zJ8OZ5S7NDMMFi2wcUkxQO9QNqndo+opOBYVMxrz2Hc2Ch2vzuSlVwUxKE60qfARFZ5ZbVOJ8Ate+vghrldgyRF7Sg5yid8Rv6RHv4nvJZpEFjmtkluWzNwhoaF9ifNdB2y7MZeDgu1n1v9xmexSYs97cCB+sprSLYrqgsk= florian@nixos-thinkpad"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCg3kPf2YxtGrfckExx/ckZrsXH1sa0mDGAAVaK6VOpLF4s0fnWJmXrBSfeGgOVPHRCNF3dfQR34u5PwO3gtK8FQr8XvVRKP+EcMi3ztKHcawnThJyB2Zi3D8yTGDYnKRXoiDvitTWtDDmZta91QJZsK65R0SeOGSk1lG6MOYyEhTRQS4rV1Ij8qEqAMB7R7/yUIkdoyGDbcDZ05Bs8/NCfBLJ/pv+pRKp+ZFIjqmKNVYznZ4OT7ywbbQaTNUCoQO4Hcm+ujlOw2jV7Xqb9842uBRQKNKik3hGw1DlAxKrbK/s3Uu0Vs/cUrEoH68+tKZdCzoX3YoYI9cyMRS2+LEkb florian@florian"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDJjjBxd9De7YH9ZCzSLmmBTXYmMOdPaLuToZc5zFe/FUz8V8NhQCmHnPP/wcL2/3gz8jz19PcXKN4jc204Nx+XrFvXPO2BMuaeztwyhLd7o5LLmWEm66PImXq4BPLUH/QTlSumIDPwnehUnorUVEcso2VSBidZ4hor2FTIvP3x3DT19xw+cH19fh6xjSEx3bYk8aQTbXpxQAduou8Q424fmOuaQcCO9f3odYmuhVhmN6hjtCK/NhcKQRQ25C1Ftw+NcC++e2c196J4VLTB3XybKU4BYPI9A1Lq8QHNS16JapGWcvoaamx5o8Br+zxmmBlTJvhMSEUga02QXMrVPkTaDDAwtzSBl1mSD9lx5haCySCH9vsjfR5aoc5Axm1qPiO+kxlLSCmdd7CxexWPR0WbmCt7toPHXGoeVHxY2p6AdeZDdgdr1ccf2iLcE7Y37cwvEBiQx0KCAN3kkYAdSBxFStUDUMtb+cbrnDAqCztg5LIr4GoT5PeXtPW2CCUl74c= root@nixos-thinkpad"
    ];
    florian.openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDIc5ug7o0Jqe6SNNrjW5BAyTWqZGVvehJ7GOZrT7DFiiop174CdDlRo4GZlvAGzFEaWr5/4knS0p8kErDPgcgdfC0IL2BYClPEna8agHhyqvmZISxfFDk48Bg/yGo37iPpuGxT7g6VIqI46PnTqgF3nfX1J3crPDD1tDUv5Nq+LH3qlwnpRA3rMBTym/QPkPAM8jQGB4DtyhI1s6UBEQK5vvljhYBG/P54ILQUokYqIsUirQKpBW7Z3sY+zezJpOc+Y6DRZ0rm9dRa6HsOFQ1DQ6u3FkBcyq+vkr4KWmxDdRO0acAV6o0c+1dqyhdaKfklO1E9ZOScTG9Wur9p17qMPsd1zJ8OZ5S7NDMMFi2wcUkxQO9QNqndo+opOBYVMxrz2Hc2Ch2vzuSlVwUxKE60qfARFZ5ZbVOJ8Ate+vghrldgyRF7Sg5yid8Rv6RHv4nvJZpEFjmtkluWzNwhoaF9ifNdB2y7MZeDgu1n1v9xmexSYs97cCB+sprSLYrqgsk= florian@nixos-thinkpad"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCg3kPf2YxtGrfckExx/ckZrsXH1sa0mDGAAVaK6VOpLF4s0fnWJmXrBSfeGgOVPHRCNF3dfQR34u5PwO3gtK8FQr8XvVRKP+EcMi3ztKHcawnThJyB2Zi3D8yTGDYnKRXoiDvitTWtDDmZta91QJZsK65R0SeOGSk1lG6MOYyEhTRQS4rV1Ij8qEqAMB7R7/yUIkdoyGDbcDZ05Bs8/NCfBLJ/pv+pRKp+ZFIjqmKNVYznZ4OT7ywbbQaTNUCoQO4Hcm+ujlOw2jV7Xqb9842uBRQKNKik3hGw1DlAxKrbK/s3Uu0Vs/cUrEoH68+tKZdCzoX3YoYI9cyMRS2+LEkb florian@florian"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDJjjBxd9De7YH9ZCzSLmmBTXYmMOdPaLuToZc5zFe/FUz8V8NhQCmHnPP/wcL2/3gz8jz19PcXKN4jc204Nx+XrFvXPO2BMuaeztwyhLd7o5LLmWEm66PImXq4BPLUH/QTlSumIDPwnehUnorUVEcso2VSBidZ4hor2FTIvP3x3DT19xw+cH19fh6xjSEx3bYk8aQTbXpxQAduou8Q424fmOuaQcCO9f3odYmuhVhmN6hjtCK/NhcKQRQ25C1Ftw+NcC++e2c196J4VLTB3XybKU4BYPI9A1Lq8QHNS16JapGWcvoaamx5o8Br+zxmmBlTJvhMSEUga02QXMrVPkTaDDAwtzSBl1mSD9lx5haCySCH9vsjfR5aoc5Axm1qPiO+kxlLSCmdd7CxexWPR0WbmCt7toPHXGoeVHxY2p6AdeZDdgdr1ccf2iLcE7Y37cwvEBiQx0KCAN3kkYAdSBxFStUDUMtb+cbrnDAqCztg5LIr4GoT5PeXtPW2CCUl74c= root@nixos-thinkpad"
    ];
  };

  age.secrets = {
    steamuser = {
      file = ./secrets/steamuser.age;
      owner = "florian";
    };
    steampass = {
      file = ./secrets/steampass.age;
      owner = "florian";
    };
  };

  environment.systemPackages = [
    (let
      user = config.age.secrets.steamuser.path;
      pass = config.age.secrets.steampass.path;
    in pkgs.writers.writeBashBin "steamLoginWrapper" ''
      user=$(cat ${user})
      pass=$(cat  ${pass})
      steam -login "$user" "$pass"
    '')
    pkgs.grub2
  ];

  hardware.enableRedistributableFirmware = true;

  nixpkgs = let system = "x86_64-linux";
  in {
    hostPlatform = { inherit system; };
    overlays = [ (_: _: { inherit system; }) ];
  };
}
