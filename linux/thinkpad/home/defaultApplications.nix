{ osConfig, lib, pkgs, ... }:

{
  # FIXME doesn't work if firefox is closed (directly executing $BROWSER works)
  # Properly the environment variables are different. (the execution in strace is the same)
  xdg = {
    mimeApps = {
      enable = true;
      defaultApplications = {
        "x-scheme-handler/https" = "linkopen.desktop";
        "x-scheme-handler/ftp" = "linkopen.desktop";
        "x-scheme-handler/chrome" = "linkopen.desktop";
        "text/html" = "linkopen.desktop";
        "application/x-extension-htm" = "linkopen.desktop";
        "application/x-extension-html" = "linkopen.desktop";
        "application/x-extension-shtml" = "linkopen.desktop";
        "application/xhtml+xml" = "linkopen.desktop";
        "application/x-extension-xhtml" = "linkopen.desktop";
        "application/x-extension-xht" = "linkopen.desktop";
        "x-scheme-handler/sgnl" = "signal-desktop.desktop";
      };
      associations.added = {
        "x-scheme-handler/http" = "linkopen.desktop";
        "x-scheme-handler/https" = "linkopen.desktop";
        "x-scheme-handler/ftp" = "linkopen.desktop";
        "x-scheme-handler/chrome" = "linkopen.desktop";
        "text/html" = "linkopen.desktop";
        "application/x-extension-htm" = "linkopen.desktop";
        "application/x-extension-html" = "linkopen.desktop";
        "application/x-extension-shtml" = "linkopen.desktop";
        "application/xhtml+xml" = "linkopen.desktop";
        "application/x-extension-xhtml" = "linkopen.desktop";
        "application/x-extension-xht" = "linkopen.desktop";
        "application/pdf" = "org.pwmt.zathura.desktop";
      };
    };
    desktopEntries = {
      linkopen =
        let
          linkopen = if osConfig.wayland.enable then pkgs.my-linkopen-wayland-or-tty else pkgs.my-linkopen-x-or-tty;
        in {
          name = "linkopen";
          exec = "${lib.getExe linkopen} %u";
          mimeType = [ "text/html" "text/xml" ];
        };
    };
  };
}
