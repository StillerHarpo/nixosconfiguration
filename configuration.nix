{ config, lib, pkgs, outputs, inputs, ... }:

{
  environment.systemPackages = with pkgs; ([
    aspell
    aspellDicts.de
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
  ]);

  fonts = {
    fontDir.enable = true;
    fonts = with pkgs; [ dejavu_fonts source-code-pro ];
  };

  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    settings.max-jobs = lib.mkDefault 8;
    registry.nixpkgs.flake = inputs.nixpkgs;
    nixPath = [ "nixpkgs=${inputs.nixpkgs.outPath}" ];
  };

  nixpkgs = {
    overlays = builtins.attrValues outputs.overlays;
    config.allowUnfree = true;
  };

}
