{ config, lib, pkgs, outputs, inputs, ... }:

{
  environment.systemPackages = [
    (pkgs.aspellWithDicts (dicts: [
      dicts.de
      dicts.en
      dicts.en-computers
      dicts.en-science
    ]))
  ];

  fonts = {
    fontDir.enable = true;
    packages = with pkgs; [ dejavu_fonts source-code-pro ];
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
