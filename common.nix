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
    enableFontDir = true;
    fonts = with pkgs; [
      dejavu_fonts
      source-code-pro
    ];
  };
  # services.postgresql.enable = true;
}
