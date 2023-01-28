{ config, lib, pkgs, ... }:

{
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
        "x-scheme-handler/mattermost" = "Mattermost.desktop";
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
      linkopen = {
        name = "linkopen";
        exec = "${(import ../scripts pkgs).linkopen}/bin/linkopen";
        terminal = true;
        mimeType = [ "text/html" "text/xml" ];
      };
    };
  };
}
