{
  lib,
  pkgs,
  outputs,
  private,
  ...
}:

{

  spelling.enable = true;

  fonts = {
    fontDir.enable = true;
    packages = with pkgs; [
      dejavu_fonts
      source-code-pro
    ];
  };

  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    settings = {
      max-jobs = lib.mkDefault 8;
      substituters = lib.mkAfter [
        "https://gradient.ts.haering.dev/cache/nixconfig"
      ];
      trusted-public-keys = [
        "gradient.ts.haering.dev-nixconfig:II5Re1ZjWdDCZBSXWC2gA35H57r5eJTRl5bv++delZ4="
        "serve:sFMUNbcn7Laygzzq7CuIT+9hgPdQSLOtDDu90uO3FnQ="
      ];
    };
  };

  nixpkgs = {
    overlays = [
      (final: prev: { inherit private; })
    ]
    ++ builtins.attrValues outputs.overlays;
    config.allowUnfree = true;
  };

}
