{ config, pkgs, ... }:

let
  python3env = pkgs.myEnvFun {
    name = "python3-env";
    buildInputs = with pkgs; ([
      python3
    ] ++
    (with python3Packages; [
      flake8 jedi
    ]));
  };

  python2env = pkgs.myEnvFun {
    name = "python2-env";
    buildInputs = with pkgs; ([
      python2
    ] ++
    (with python2Packages; [
      flake8 jedi
    ]));
  };
   
  xmonadenv = pkgs.myEnvFun {
    name = "xmonad-env";
    buildInputs = [
      (pkgs.haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
        xmonad xmonad-contrib xmonad-extras
        hdevtools hlint ghc-mod
      ]))
    ]; 
  };

  compilerbauenv = pkgs.myEnvFun {
    name = "compilerbau-env";
    buildInputs = [
      (pkgs.haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
        alex happy
        hdevtools hlint ghc-mod
      ]))
    ]; 
  };

  writeschemeenv= pkgs.myEnvFun {
    name = "writeScheme-env";
    buildInputs = [
      (pkgs.haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
        parsec 
        hdevtools hlint ghc-mod
      ]))
    ]; 
  };

  idrisenv = pkgs.myEnvFun {
    name = "idris-env";
    buildInputs = [
      (pkgs.haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
        idris
      ]))
    ]; 
  };

  renv = pkgs.myEnvFun {
    name = "r-env";
    buildInputs = with pkgs; ([
      R 
    ] ++
    (with rPackages; [
      MASS xtable tikzDevice
    ]));
  }; 

in

{
  environment.systemPackages = [ python3env python2env 
                                 xmonadenv compilerbauenv writeschemeenv
                                 renv 
                                 idrisenv
                               ];
}  
