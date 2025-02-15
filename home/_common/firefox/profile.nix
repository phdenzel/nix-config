{config, ...}: {
  imports = [./betterfox.nix];

  home.file = {
    ".mozilla/assets/logo/arxiv.png".source = ../../../assets/mozilla/arxiv.png;
  };

  programs.firefox = {
    profiles."${config.home.username}" = {
      id = 0;
      isDefault = true;

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
      containersForce = true;

      settings = {
        # User settings
        "apz.overscroll.enabled" = true; # smooth scrolling
        "general.smoothScroll" = true;

        "privacy.userContext.enabled" = true; # enable container tabs

        "browser.newtabpage.activity-stream.feeds.topsites" = true; # add new tab topsites rows
        "browser.newtabpage.activity-stream.topSitesRows" = 2; # two rows
        "browser.newtabpage.pinned" = [
          {url = "https://google.com"; label = "@google"; searchTopSite = true; baseDomain = "google.com";}
          {url = "https://phdenzel.github.io";}
          {url = "http://jellyfin.home/";}
          {url = "http://nextcloud.home/";}
          {url = "http://freshrss.home:9877/";}
          {url = "https://mail.proton.me/";}
          {url = "https://www.youtube.com";}
          {url = "https://open.spotify.com/";}

          {url = "https://arxiv.org/search/"; label = "@arxiv"; searchTopSite = false;}
          {url = "https://github.com/phdenzel";}
          {url = "https://gitlab.com/phdenzel";}
          {url = "https://www.overleaf.com/project";}
          {url = "https://account.cscs.ch/ump";}
        ];
        "identity.fxaccounts.enabled" = false; # disable Firefox Sync
        "signon.rememberSignons" = false; # disable password manager
        "extensions.formautofill.creditCards.enabled" = false; # disable credit card manager
        
        # "browser.search.region" = "US";
      };
      # extensios = {
      # };
    };
  };
}
