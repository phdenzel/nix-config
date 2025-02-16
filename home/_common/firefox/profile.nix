{
  pkgs,
  config,
  ...
}: {
  imports = [./betterfox.nix];

  home.file = {
    ".mozilla/assets/logo/arxiv.png".source = ../../../assets/mozilla/arxiv.png;
  };

  programs.firefox = {
    profiles."${config.home.username}" = {
      id = 0;
      isDefault = true;

      extensions = with pkgs.inputs.firefox-addons; [
        browserpass
        darkreader
        simple-tab-groups
        ublock-origin
        xbrowsersync
      ];
      settings."extensions.autoDisableScopes" = 0; # auto enable addons
      settings."extensions.enabledScopes" = 15; # auto enable addons

      search = {
        default = "DuckDuckGo";
        force = true;
        order = ["DuckDuckGo" "Google" "arXiv" "Wikipedia" "Nix Packages" "NixOS Wiki"];
        engines = {
          "Nix Packages" = {
            urls = [
              {
                template = "https://search.nixos.org/packages";
                params = [
                  {
                    name = "type";
                    value = "packages";
                  }
                  {
                    name = "query";
                    value = "{searchTerms}";
                  }
                ];
              }
            ];
            iconUpdateURL = "https://search.nixos.org/favicon.png";
            updateInterval = 24 * 60 * 60 * 1000; # every day
            definedAliases = ["@np"];
          };
          "NixOS Wiki" = {
            urls = [{template = "https://wiki.nixos.org/index.php?search={searchTerms}";}];
            iconUpdateURL = "https://search.nixos.org/favicon.png";
            updateInterval = 24 * 60 * 60 * 1000; # every day
            definedAliases = ["@nw"];
          };
          "arXiv" = {
            urls = [
              {
                template = "https://arxiv.org/search/";
                params = [
                  {
                    name = "query";
                    value = "{searchTerms}";
                  }
                  {
                    name = "searchtype";
                    value = "all";
                  }
                  {
                    name = "abstracts";
                    value = "show";
                  }
                  {
                    name = "order";
                    value = "announced_date_first";
                  }
                  {
                    name = "size";
                    value = "50";
                  }
                ];
              }
            ];
            icon = "${config.home.homeDirectory}/.mozilla/assets/logo/arxiv.png";
            definedAliases = ["@a" "@arxiv"];
          };
          "Bing".metaData.hidden = true;
          "DuckDuckGo".metaData.alias = "@d";
          "Google".metaData.alias = "@g";
          "Wikipedia".metaData.alias = "@w";
        };
      };

      containers = {
        personal = {
          color = "blue";
          icon = "fingerprint";
          id = 1;
        };
        work = {
          color = "orange";
          icon = "briefcase";
          id = 2;
        };
        banking = {
          color = "green";
          icon = "dollar";
          id = 3;
        };
        shopping = {
          color = "pink";
          icon = "cart";
          id = 4;
        };
        dev = {
          color = "turquoise";
          icon = "chill";
          id = 5;
        };
        research = {
          color = "red";
          icon = "chill";
          id = 6;
        };
        mixed = {
          color = "yellow";
          icon = "gift";
          id = 7;
        };
      };
      settings."privacy.userContext.enabled" = true;
      containersForce = true;

      settings = {
        # User configuration
        "browser.download.useDownloadDir" = false;

        # UI
        "browser.download.panel.shown" = true;
        "browser.display.use_system_colors" = true;
        "browser.uiCustomization.state" = {
          dirtyAreaCache = ["unified-extensions-area" "nav-bar" "vertical-tabs" "PersonalToolbar"];
          currentVersion = 20;
          newElementCount = 10;
          placements = {  
            TabsToolbar = ["tabbrowser-tabs" "new-tab-button" "alltabs-button"];
            # TabsToolbar = ["firefox-view-button" "tabbrowser-tabs" "new-tab-button" "alltabs-button"];
            nav-bar = ["back-button" "forward-button" "stop-reload-button" "urlbar-container" "save-to-pocket-button" "home-button" "downloads-button" "unified-extensions-button" "fxa-toolbar-menu-button" "reset-pbm-toolbar-button" "ublock0_raymondhill_net-browser-action" "addon_darkreader_org-browser-action" "browserpass_maximbaz_com-browser-action" "simple-tab-groups_drive4ik-browser-action"];
            toolbar-menubar = ["menubar-items"];
            PersonalToolbar = ["personal-bookmarks"];
            unified-extensions-area = ["_019b606a-6f61-4d01-af2a-cea528f606da_-browser-action"];
            widget-overflow-fixed-list = [];
            vertical-tabs = [];
          };
          seen = ["reset-pbm-toolbar-button" "browserpass_maximbaz_com-browser-action" "addon_darkreader_org-browser-action" "simple-tab-groups_drive4ik-browser-action" "ublock0_raymondhill_net-browser-action" "_019b606a-6f61-4d01-af2a-cea528f606da_-browser-action" "developer-button"];
        };

        # Behaviour
        "browser.startup.homepage" = "about:home";
        "apz.overscroll.enabled" = true; # smooth scrolling
        "general.smoothScroll" = true;
        "privacy.trackingprotection.enabled" = true; # tracking protection
        "identity.fxaccounts.enabled" = false; # disable Firefox Sync
        "signon.rememberSignons" = false; # disable save password prompt
        "extensions.formautofill.creditCards.enabled" = false; # disable credit card manager

        # New Tab Page
        "browser.newtabpage.activity-stream.feeds.topsites" = true; # add new tab topsites rows
        "browser.newtabpage.activity-stream.topSitesRows" = 2; # two rows
        "browser.newtabpage.pinned" = [
          {
            url = "https://google.com";
            label = "@google";
            searchTopSite = true;
            baseDomain = "google.com";
          }
          {url = "https://phdenzel.github.io";}
          {url = "http://jellyfin.home/";}
          {url = "http://nextcloud.home/";}
          {
            url = "http://localhost:9091/";
            label = "tx";
          }
          {
            url = "https://mail.proton.me/";
            label = "proton";
          }
          {url = "https://www.youtube.com";}
          {
            url = "https://open.spotify.com/";
            label = "spotify";
          }

          {
            url = "https://arxiv.org/search/";
            label = "@arxiv";
            searchTopSite = false;
          }
          {url = "https://github.com/phdenzel";}
          {url = "https://gitlab.com/phdenzel";}
          {url = "https://www.overleaf.com/project";}
          {url = "https://wandb.ai/home";}
          {
            url = "http://localhost:8080";
            label = "ollama";
          }
          {
            url = "http://localhost:8000/";
            label = "jupyter";
          }
          {
            url = "https://account.cscs.ch/ump";
            label = "cscs";
          }
        ];
      };
    };
  };
}
