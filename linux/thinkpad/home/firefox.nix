{ config, lib, pkgs, ... }:

{
  programs.firefox = {
    enable = true;
    profiles = let
      extensions = with pkgs.nur.repos.rycee.firefox-addons;
        let
          ffreszoom = buildFirefoxXpiAddon rec {
            pname = "ffreszoom";
            version = "0.4";
            addonId = "{b2e3360c-a72c-4ba4-813c-603a1fa34356}";
            url =
              "https://addons.mozilla.org/firefox/downloads/file/1712974/ffreszoom-${version}-fx.xpi";
            sha256 = "sha256-lg2PvsD+5rIC33D2yE3xqloPGXNbjCg/befN2E0HEB0=";

            meta = with lib; {
              homepage = "https://github.com/notartom/ffreszoom";
              description =
                "Sets the zoom level based on the screen resolution.";
              license = licenses.gpl3;
              platforms = platforms.all;
            };
          };
          german-dictonary = buildFirefoxXpiAddon rec {
            pname = "german_dictonary";
            version = "2.1";
            addonId = "de-DE@dictionaries.addons.mozilla.org";
            url =
              "https://addons.mozilla.org/firefox/downloads/file/4034565/dictionary_german-${version}.xpi";
            sha256 = "sha256-AO9us8EBcah/siq25RaEZni3PFaugozBnRHjLkO4RXo=";
            meta = with lib; {
              homepage =
                "https://addons.mozilla.org/en-US/firefox/addon/dictionary-german/";
              description = "German Dictionary";
              license = licenses.gpl2;
              platforms = platforms.all;
            };
          };
        in [
          user-agent-string-switcher
          tridactyl
          noscript
          ublock-origin
          ffreszoom
          german-dictonary
        ];
      commonSettings = {
        # colors
        "widget.content.allow-gtk-dark-theme" = true;
        "browser.display.use_system_colors" = true;

        "browser.urlbar.placeholderName" = "DuckDuckGo";

        # Enable HTTPS-Only Mode
        "dom.security.https_only_mode" = true;
        "dom.security.https_only_mode_ever_enabled" = true;

        # Privacy settings
        "privacy.donottrackheader.enabled" = true;
        "privacy.trackingprotection.enabled" = true;
        "privacy.trackingprotection.socialtracking.enabled" = true;
        "privacy.partition.network_state.ocsp_cache" = true;

        # Disable all sorts of telemetry
        "browser.newtabpage.activity-stream.feeds.telemetry" = false;
        "browser.newtabpage.activity-stream.telemetry" = false;
        "browser.ping-centre.telemetry" = false;
        "toolkit.telemetry.archive.enabled" = false;
        "toolkit.telemetry.bhrPing.enabled" = false;
        "toolkit.telemetry.enabled" = false;
        "toolkit.telemetry.firstShutdownPing.enabled" = false;
        "toolkit.telemetry.hybridContent.enabled" = false;
        "toolkit.telemetry.newProfilePing.enabled" = false;
        "toolkit.telemetry.reportingpolicy.firstRun" = false;
        "toolkit.telemetry.shutdownPingSender.enabled" = false;
        "toolkit.telemetry.unified" = false;
        "toolkit.telemetry.updatePing.enabled" = false;

        # As well as Firefox 'experiments'
        "experiments.activeExperiment" = false;
        "experiments.enabled" = false;
        "experiments.supported" = false;
        "network.allow-experiments" = false;

        # Disable Pocket Integration
        "browser.newtabpage.activity-stream.section.highlights.includePocket" =
          false;
        "extensions.pocket.enabled" = false;
        "extensions.pocket.api" = "";
        "extensions.pocket.oAuthConsumerKey" = "";
        "extensions.pocket.showHome" = false;
        "extensions.pocket.site" = "";
      };
    in {
      default = {
        settings = commonSettings;
        inherit extensions;
      };
      work = {
        id = 1;
        settings = commonSettings;
        inherit extensions;
      };
    };
  };

}
