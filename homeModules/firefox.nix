{ config, lib, pkgs, ... }:

let 
  cfg = config.firefox;
  firefox-addons =  pkgs.nur.repos.rycee.firefox-addons;
  ffreszoom = let version = "0.4";
    in firefox-addons.buildFirefoxXpiAddon {
    pname = "ffreszoom";
    inherit version;
    addonId = "{b2e3360c-a72c-4ba4-813c-603a1fa34356}";
    url =
      "https://addons.mozilla.org/firefox/downloads/file/1712974/ffreszoom-${version}-fx.xpi";
    sha256 = "sha256-lg2PvsD+5rIC33D2yE3xqloPGXNbjCg/befN2E0HEB0=";

    meta = {
      homepage = "https://github.com/notartom/ffreszoom";
      description =
        "Sets the zoom level based on the screen resolution.";
      license = lib.licenses.gpl3;
      platforms = lib.platforms.all;
    };
  };
  german-dictonary = let version = "2.1";
    in firefox-addons.buildFirefoxXpiAddon {
    pname = "german_dictonary";
    inherit version;
    addonId = "de-DE@dictionaries.addons.mozilla.org";
    url =
      "https://addons.mozilla.org/firefox/downloads/file/4034565/dictionary_german-${version}.xpi";
    sha256 = "sha256-AO9us8EBcah/siq25RaEZni3PFaugozBnRHjLkO4RXo=";
    meta = {
      homepage =
        "https://addons.mozilla.org/en-US/firefox/addon/dictionary-german/";
      description = "German Dictionary";
      license = lib.licenses.gpl2;
      platforms = lib.platforms.all;
    };
  };
  extensions = 
    with firefox-addons; [
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
  options.firefox = {
    enable = lib.mkEnableOption (lib.mdDoc "Wether to use firefox");
    workProfile = lib.mkEnableOption (lib.mdDoc "Wether to install a firefox work profie");
  };
  config = lib.mkIf cfg.enable {
    programs.firefox = {
      enable = true;
      profiles = lib.mkMerge [
        {
          default = {
            settings = commonSettings;
            inherit extensions;
          };
        }
        (lib.mkIf cfg.workProfile {
          work = {
            id = 1;
            settings = commonSettings;
            inherit extensions;
          };
        })
      ];
    };
  };
}
