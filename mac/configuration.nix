{ config, lib, pkgs, defaultShell, ... }:
let
  home-manager = builtins.fetchGit {
    url = "https://github.com/rycee/home-manager.git";
    ref = "release-20.09";
  };
in
{

  imports = [
    ../configuration.nix (import "${home-manager}/nix-darwin")
  ];

  environment.systemPackages = with pkgs; ([
    git
    slack
    niv
    pass
    bashInteractive_5
    haskell-language-server
  ]);

  users.users.florianengel = {
    name = "florianengel";
    home = "/Users/florianengel";
  };

  home-manager.users.florianengel = import ./home.nix defaultShell;

  nixpkgs.config = {
    # Allow proprietary packages
    allowUnfree = true;
    overlays = [
      (self: super: {
         bashInteractive = super.bashInteractive_5;
      })
    ];
  };

  # environment.shells = with pkgs; [ zsh bashInteractive ];

  programs = {
    zsh.enable = true;
   # bash.enable = true;
#    gnupg.enable = true;
  };

  system.stateVersion = 4;
}
