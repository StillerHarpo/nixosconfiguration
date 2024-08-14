{ config, lib, pkgs, outputs, inputs, ... }:

{
  environment.systemPackages = [
     pkgs.myAspellWithDicts
  ];

  fonts = {
    fontDir.enable = true;
    packages = with pkgs; [ dejavu_fonts source-code-pro ];
  };

  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    settings = {
      max-jobs = lib.mkDefault 8;
      substituters = lib.mkAfter [
        "https://cache.ts.haering.dev?priority=39"
      ];
      trusted-public-keys = [
        "cache.ts.haering.dev:II5Re1ZjWdDCZBSXWC2gA35H57r5eJTRl5bv++delZ4="
      ];
    };
  };

  nixpkgs = {
    overlays = builtins.attrValues outputs.overlays;
    config.allowUnfree = true;
  };

}
