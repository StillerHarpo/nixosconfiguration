{
  config,
  pkgs,
  lib,
  ...
}:

{
  options.myUnprivilegedTerminal = lib.mkOption {
    default =
      pkgs.runCommand "unprivileged-alacritty"
        {
          nativeBuildInputs = [ pkgs.makeBinaryWrapper ];
        }
        ''
          makeBinaryWrapper ${lib.getExe pkgs.alacritty} $out --argv0 unprivileged-alacritty 
        '';
  };
  config = {
    security.apparmor.policies.unprivilegedTerminal.profile = ''
      ${config.myUnprivilegedTerminal} {
       ${lib.apparmor.generateFileRules {
         inherit config;
         fileDefault = "/** Pixmrwlk";
       }}
      }
    '';
  };
}
