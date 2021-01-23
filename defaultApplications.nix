{ config, lib, pkgs, ... }:

let
  defaultApplication = {mimetype, path, name, options ? ""}:
    {
      home.file.".local/share/applications/${name}.desktop".text = ''
        [Desktop Entry]
        Encoding=UTF-8
        Version=1.0
        Type=Application
        Terminal=false
        Exec=${path}/${name} ${options}
        Name=Name of Application
        Icon=/path/to/icon
     '';
      xdg.mimeApps.defaultApplications = { "${mimetype}" = "${name}.desktop"; };
    };
  defaultEmacsClient =
    mimetype: defaultApplication {inherit mimetype;
                                    path = "${pkgs.emacs}/bin";
                                    name = "emacsclient";
                                    options = "-c"; };
in
with lib.attrsets; {
  home-manager.users.florian =
    recursiveUpdate
      (defaultEmacsClient  "inode/directory" )
       {
         xdg.mimeApps = {
           enable = true;
           defaultApplications =
             {
               "x-scheme-handler/https" = "firefox.desktop";
               "x-scheme-handler/ftp" = "firefox.desktop";
               "x-scheme-handler/chrome" = "firefox.desktop";
               "text/html" = "firefox.desktop";
               "application/x-extension-htm" = "firefox.desktop";
               "application/x-extension-html" = "firefox.desktop";
               "application/x-extension-shtml" = "firefox.desktop";
               "application/xhtml+xml" = "firefox.desktop";
               "application/x-extension-xhtml" = "firefox.desktop";
               "application/x-extension-xht" = "firefox.desktop";
               "x-scheme-handler/sgnl" = "signal-desktop.desktop";
               "x-scheme-handler/mattermost" = "Mattermost.desktop";
             };
           associations.added =
             let browsers = [
               "userapp-Nightly-XV6LWY.desktop"
               "userapp-Nightly-XJA5XY.desktop"
               "firefox.desktop"
               ];
             in {
             "x-scheme-handler/http" = browsers;
             "x-scheme-handler/https" = browsers;
             "x-scheme-handler/ftp" = browsers;
             "x-scheme-handler/chrome" = browsers;
             "text/html" = browsers;
             "application/x-extension-htm" = browsers;
             "application/x-extension-html" = browsers;
             "application/x-extension-shtml" = browsers;
             "application/xhtml+xml" = browsers;
             "application/x-extension-xhtml" = browsers;
             "application/x-extension-xht" = browsers;
             "application/pdf" = "org.pwmt.zathura.desktop";
           };
         };
       };
}
