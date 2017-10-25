{ config, pkgs, ... }:

let
  python3env = pkgs.myEnvFun {
    name = "python3env";
    buildInputs = with pkgs; ([
      python3
    ] ++
    (with python3Packages; [
      flake8 jedi
    ]));
  };
in
{
  environment.systemPackages = [ python3env ];
}  
