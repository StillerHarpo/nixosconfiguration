{ config, lib, pkgs, ... }:

{

  environment.systemPackages = with pkgs; ([
      slack
      zoom-us
      niv
  ]);

  home-manager.users.florian = import ./homeMacSpecific.nix;

  nixpkgs.config = {
    # Allow proprietary packages
    allowUnfree = true;
  };
}
