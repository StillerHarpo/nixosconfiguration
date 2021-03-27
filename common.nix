{ config, lib, pkgs, ... }:

let
  home-manager = builtins.fetchGit {
    url = "https://github.com/rycee/home-manager.git";
    ref = "release-20.09";
  };
in
{
  imports = [
    (import "${home-manager}/nixos")
  ];

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
      terminus_font
      dejavu_fonts
      source-code-pro
    ];
  };

  services.postgresql.enable = true;
}
