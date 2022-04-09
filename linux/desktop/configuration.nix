{ config, pkgs, ... }:

{

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    initrd.availableKernelModules = [ "ahci" "ohci_pci" "ehci_pci" "xhci_pci" "usbhid" "usb_storage" "sd_mod" "sr_mod" ];
    kernelModules = [ "kvm-amd" ];
  };

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/550f9ea8-74b0-4775-ac06-f178089dc9a4";
      fsType = "ext4";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/db9038db-4602-4d7e-9a4f-5d56f43c8db5"; }
    ];
  networking = {
    useDHCP = false;
    interfaces.enp10s0.useDHCP = true;
    firewall.allowedTCPPorts = [ 22 ];
  };

  # Enable SSH in the boot process.
  services.openssh = {
    enable = true;
    permitRootLogin = "yes";
  };
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDIc5ug7o0Jqe6SNNrjW5BAyTWqZGVvehJ7GOZrT7DFiiop174CdDlRo4GZlvAGzFEaWr5/4knS0p8kErDPgcgdfC0IL2BYClPEna8agHhyqvmZISxfFDk48Bg/yGo37iPpuGxT7g6VIqI46PnTqgF3nfX1J3crPDD1tDUv5Nq+LH3qlwnpRA3rMBTym/QPkPAM8jQGB4DtyhI1s6UBEQK5vvljhYBG/P54ILQUokYqIsUirQKpBW7Z3sY+zezJpOc+Y6DRZ0rm9dRa6HsOFQ1DQ6u3FkBcyq+vkr4KWmxDdRO0acAV6o0c+1dqyhdaKfklO1E9ZOScTG9Wur9p17qMPsd1zJ8OZ5S7NDMMFi2wcUkxQO9QNqndo+opOBYVMxrz2Hc2Ch2vzuSlVwUxKE60qfARFZ5ZbVOJ8Ate+vghrldgyRF7Sg5yid8Rv6RHv4nvJZpEFjmtkluWzNwhoaF9ifNdB2y7MZeDgu1n1v9xmexSYs97cCB+sprSLYrqgsk= florian@nixos-thinkpad"
  ];

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Configure keymap in X11
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "eurosign:e";

  # Enable sound.
  sound.enable = true;
  hardware = {
    enableRedistributableFirmware = true;
    pulseaudio.enable = true;
  };

  programs.steam.enable = true;


  nixpkgs.config.allowUnfree = true;
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

}
