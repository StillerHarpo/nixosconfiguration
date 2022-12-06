{ config, lib, pkgs, ... }:

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
  };

}
