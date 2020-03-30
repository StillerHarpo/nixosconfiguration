{ config, pkgs, ...}:
with pkgs;
with builtins;
foldl' (init: container:
  with container;
  let
    oldPackages = if init == {} then [] else init.environment.systemPackages;
    oldIpRanges = if init == {} then [ "127.0.0.1" ] else init.environment.systemPackages;
    oldContainers = if init == {} then {} else init.containers;
    run = pkgs.writeScriptBin "run-${name}" ''
    #!/bin/sh
    ${socat}/bin/socat -d TCP-LISTEN:6000,fork,bind=192.168.${addresses}.10 UNIX-CONNECT:/tmp/.X11-unix/X0 &
    ${xorg.xhost}/bin/xhost +
    ${openssh}/bin/ssh -i ~/.ssh/nixos-containers -X ${name}@192.168.${addresses}.11 run-${name}
  '';
  in
    {
      environment.systemPackages = oldPackages ++ [ run ];
      hardware.pulseaudio.tcp.tcp.anonymousClients.allowedIpRanges = [ "192.168.${addresses}.0/24"] ++ oldIpRanges;
      containers = oldContainers // {
        "${name}" = {
          autoStart = true;
          privateNetwork = true;
          hostAddress = "192.168.${addresses}.10";
          localAddress = "192.168.${addresses}.11";
          config = ({config, pkgs, ... }:
            with pkgs;
            let run = pkgs.writeScriptBin "run-${name}" ''
          #!/bin/sh
          PULSE_SERVER=tcp:192.168.${addresses}.10:4713 XAUTHORITY="/home/browser/.Xauthority" DBUS_SESSION_BUS_ADDRESS="" DISPLAY=192.168.${addresses}.10:0.0 apulse ${executable} $@
        '';
            in {

              environment.systemPackages = [ run apulse ];

              services.openssh = {
                enable = true;
                forwardX11 = true;
              };

              users.extraUsers."${name}" = {
                isNormalUser = true;
                home = "/home/browser";
                openssh.authorizedKeys.keys = [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDHDLf0BWTHBod3jo/GoEN4IpAhT/WNyo3/Xng3Ph9+ckR3qxVzQ3GopfxyZkWYOnD/n38x9kaMdpGUWlBqbgcl1jpm9mDavNibWFrdu9K6qXxT2lIVT+nQLVt1Dd9i5ZIMAGfF3Sg9+ahctoqprjfRTj+DAh/oCRaSOLkSkXFbDTlVgIpEQl0+LxHhQ3LaIpg9WlI+bQRTuJpr4UOdLKZyX9nf+4jIrZZ00HlPwHmXYNq3//e8wEd/lJh+gBAt/eT6FYXmiwS8QIsOrz2uBKSwblizv5kyxJ8+WpbUVTK7Dpcv7mMJOZSnPienmMC81albGL9XQ74JkXwdppj0jeeL florian@nixos" ];
                extraGroups = ["audio" "video"];
              };
            });
        };
      };
    }) {} [
      {
        name = "tor";
        addresses = "9";
        executable = "${tor-browser-bundle-bin}/bin/tor-browser";
      }
      {
        name = "spotify";
        addresses = "8";
        executable = "${spotify}/bin/spotify";
      }
      {
        name = "firefox";
        addresses = "7";
        executable = "${firefox}/bin/firefox";
      }
    ]
// {
  networking = {
    firewall = {
      allowedTCPPorts = [ 4713 6000 ];
      extraCommands = "iptables -t nat -A POSTROUTING -o wlp61s0 -j MASQUERADE";
    };
    nat = {
      enable = true;
      internalInterfaces = ["ve-+" "enp0s31f6"];
      externalInterface = "wlp61s0";
    };
    networkmanager.unmanaged = [ "interface-name:ve-*" ];
  };
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.systemWide = true;
  hardware.pulseaudio.support32Bit = true;
  hardware.pulseaudio.tcp.enable = true;
  boot.kernel.sysctl."net.ipv4.ip_forward" = 1;
}
