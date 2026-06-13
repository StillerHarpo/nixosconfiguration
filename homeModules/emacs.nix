{
  config,
  lib,
  pkgs,
  osConfig,
  ...
}:
let
  cfg = config.emacs;
in
{
  options.emacs = {
    enable = lib.mkEnableOption (lib.mdDoc "Whether to install emacs");
    base = lib.mkOption {
      type = with lib.types; nullOr package;
      default = null;
      description = lib.mdDoc "the base emacs package to use";
    };
    package = lib.mkOption {
      type = with lib.types; nullOr package;
      default = null;
      description = lib.mdDoc "the emacs package to use";
    };
  };

  config =
    let
      base =
        if cfg.base != null then
          cfg.base
        else if osConfig.xserver.enable then
          pkgs.emacs-unstable
        else if osConfig.wayland.enable then
          pkgs.emacs-git-pgtk
        else
          pkgs.emacs-unstable-nox;
      package = pkgs.emacsWithPackagesFromUsePackage {
        config = ./config.el;
        defaultInitFile = true;
        alwaysEnsure = true;
        package = base;
        extraEmacsPackages = epkgs: [
          epkgs.treesit-grammars.with-all-grammars
          epkgs.pdf-tools
        ];
        # TODO remove this, after it is fixed upstream
        override = (
          final: prev: {
            rustic = prev.rustic.overrideAttrs {
              buildInputs = prev.empv.buildInputs ++ [ final.flycheck ];
            };
            linkopen = pkgs.linkopen.override { isWayland = !osConfig.xserver.enable; };
          }
        );
      };
    in
    lib.mkIf config.emacs.enable {
      services.emacs = {
        enable = true;
        inherit package;
        defaultEditor = true;
        client.enable = true;
        startWithUserSession = "graphical";
      };
      programs.emacs = {
        enable = true;
        inherit package;
      };
    };
}
