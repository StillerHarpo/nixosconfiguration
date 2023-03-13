{ config, lib, pkgs, blocklist, ... }:
with lib;
let cfg = config.dnscrypt;
in {
  options = { dnscrypt.enable = mkEnableOption (mdDoc "Enable dnscript"); };
  config = mkIf cfg.enable {
    services.dnscrypt-proxy2 = {
      enable = true;
      settings = {
        ipv6_servers = true;
        require_dnssec = true;

        sources.public-resolvers = {
          urls = [
            "https://raw.githubusercontent.com/DNSCrypt/dnscrypt-resolvers/master/v3/public-resolvers.md"
            "https://download.dnscrypt.info/resolvers-list/v3/public-resolvers.md"
          ];
          # can't use default because it is in nix store. Should be fixed upstream
          cache_file = "/var/lib/dnscrypt-proxy2/public-resolvers.md";
          minisign_key =
            "RWQf6LRCGA9i53mlYecO4IzT51TGPpvWucNSCh1CBM0QTaLn73Y7GFO3";
        };

        blocked_names.blocked_names_file = blocklist;
      };
    };

    networking = {
      dhcpcd.extraConfig = "nohook resolv.conf";
      networkmanager.dns = "none";
    };
  };
}
