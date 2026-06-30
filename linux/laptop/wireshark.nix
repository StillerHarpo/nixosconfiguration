{ pkgs, ... }:

{
  users.users.florian.extraGroups = [ "wireshark" ];
  programs.wireshark = {
    enable = true;
    package = pkgs.wireshark;
  };
}
