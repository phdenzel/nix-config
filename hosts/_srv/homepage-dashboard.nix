{
  pkgs,
  config,
  lib,
  inputs,
  ...
}: let
  background_image = "homepage-dashboard/assets/background.png";
in {
  environment.etc = {
    "${background_image}" = {
      source = "${inputs.phd-wallpapers}/serenity_4k.png";
    };
  };
  services.homepage-dashboard = {
    enable = true;
    environmentFile = config.sops.secrets."homepage-dashboard/env".path;
    package = pkgs.homepage-dashboard.overrideAttrs (attrs: {
      postInstall = ''
        mkdir -p $out/share/homepage/public/images
        ln -s /etc/${background_image} $out/share/homepage/public/images/background.png
      '';
    });
    listenPort = 8082;
    openFirewall = true;
    allowedHosts =
      lib.strings.concatStringsSep ","
      [
        "localhost:8082"
        "127.0.0.1:8082"
        "ygdrasil.home"
        "192.168.178.42"
        "heimdall.home"
        "192.168.178.64"
      ];
    settings = {
      title = "Denzel's homepage dashboard";
      description = "Home dashboard listing hosts and services.";
      # startUrl = "";
      background = "/images/background.png";
      statusStyle = "dot";
      quicklaunch = {
        searchDescriptions = true;
        showSearchSuggestions = true;
        hideInternetSearch = true;
        provider = "duckduckgo";
      };
      layout = {
        Hosts = {
          style = "row";
          columns = 4;
          useEqualHeights = true;
        };
        Bookmarks = {
          style = "row";
          columns = 4;
          useEqualHeights = true;
        };
      };
    };
    widgets = [
      {
        logo = {
          icon = "https://raw.githubusercontent.com/phdenzel/nix-config/refs/heads/main/assets/logos/phd-ark-logo.svg";
        };
      }
      {
        greeting = {
          text_size = "x1";
          text = "Denzel's homepage";
        };
      }
      {
        datetime = {
          text_size = "x1";
          format = {
            hour12 = false;
            dateStyle = "long";
            timeStyle = "short";
          };
        };
      }
      {
        openmeteo = {
          label = "Zurich";
          timezone = "Europe/Zurich";
          latitude = "{{HOMEPAGE_VAR_LATITUDE}}";
          longitude = "{{HOMEPAGE_VAR_LONGITUDE}}";
          units = "metric";
        };
      }
      { search = {
          provider = "duckduckgo";
          target = "_blank";
        };
      }
    ];
    services = [
      {
        Hosts = [
          {
            ygdrasil = {
              description = "NAS server";
              siteMonitor = "http://ygdrasil.home:61208";
              widget = {
                version = 4;
                type = "glances";
                url = "http://ygdrasil.home:61208";
                metric = "info";
                refreshInterval = 5000;
              };
            };
          }
          {
            heimdall = {
              description = "DNS server";
              siteMonitor = "http://heimdall.home:61208";
              widget = {
                version = 4;
                type = "glances";
                url = "http://heimdall.home:61208";
                metric = "info";
                refreshInterval = 5000;
              };
            };
          }
          {
            phinix = {
              description = "phdenzel's workstation";
              siteMonitor = "http://phinix.home:61208";
              widget = {
                version = 4;
                type = "glances";
                url = "http://phinix.home:61208";
                metric = "info";
                refreshInterval = 5000;
              };
            };
          }
        ];
      }
    ];
    bookmarks = [
      {
        Bookmarks = [
          {
            GitHub = [
              {
                abbr = "GH";
                href = "https://github.com/phdenzel";
                icon = "sh-github-light";
              }
            ];
          }
          {
            WandB = [
              {
                abbr = "W&B";
                href = "https://wandb.ai/home";
                icon = "https://raw.githubusercontent.com/wandb/assets/refs/heads/main/wandb-dots-logo.svg";
              }
            ];
          }
          {
            CSCS = [
              {
                abbr = "CSCS Account";
                href = "https://account.cscs.ch/ump/app#/";
                icon = "https://upload.wikimedia.org/wikipedia/commons/f/fe/Swiss_National_Supercomputing_Centre_logo.svg";
              }
            ];
          }
          {
            "CSCS JLab" = [
              {
                abbr = "CSCS JLab";
                href = "https://jupyter.cscs.ch/hub/spawn";
                icon = "sh-jupyter";
              }
            ];
          }
          {
            Dreamworld = [
              {
                abbr = "Dreamworld";
                href = "https://phdenzel.github.io/";
                icon = "https://raw.githubusercontent.com/phdenzel/nix-config/refs/heads/main/assets/logos/phd-ark-logo.svg";
              }
            ];
          }
        ];
      }
    ];
  };
}
