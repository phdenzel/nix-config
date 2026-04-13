{
  pkgs,
  config,
  lib,
  inputs,
  ...
}: let
  background_image = "homepage-dashboard/assets/background.png";
  host = "${config.networking.hostName}.home";
  mkUrl = port: "http://${host}:${toString port}";
  mkProxyUrl = service: "http://${service}.home";
in {
  environment.etc = {
    "${background_image}" = {
      source = "${inputs.phd-wallpapers}/serenity_4k.png";
    };
  };

  sops-host.keys = ["homepage-dashboard/env"];

  services.homepage-dashboard = {
    enable = true;
    environmentFiles = [config.sops.secrets."homepage-dashboard/env".path];
    package = pkgs.homepage-dashboard.overrideAttrs (attrs: {
      postInstall = ''
        mkdir -p $out/share/homepage/public/images
        ln -s /etc/${background_image} $out/share/homepage/public/images/background.png
      '';
    });
    listenPort = 8082;
    # openFirewall = true;
    allowedHosts =
      lib.strings.concatStringsSep ","
      [
        "localhost:8082"
        "127.0.0.1:8082"
        "${config.networking.hostName}.home:8082"
        "${config.networking.hostName}.home"
        "denzels.home"
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
        Services = {
          style = "row";
          columns = 4;
          useEqualHeights = true;
        };
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
      {
        search = {
          provider = "duckduckgo";
          target = "_blank";
        };
      }
    ];
    services = [
      {
        Services = [
          {
            Traefik = {
              icon = "traefik.png";
              description = "Reverse proxy & load balancer";
              href = mkProxyUrl "traefik";
              siteMonitor = mkProxyUrl "traefik";
              widget = {
                type = "traefik";
                url = mkUrl 8080;
              };
            };
          }
          {
            Forgejo = {
              icon = "forgejo.png";
              description = "Git forge";
              href = mkProxyUrl "forgejo";
              siteMonitor = mkProxyUrl "forgejo";
              widget = {
                type = "gitea";
                url = mkUrl config.services.forgejo.settings.server.HTTP_PORT;
                token = "{{HOMEPAGE_VAR_FORGEJO_TOKEN}}";
              };
            };
          }
          {
            Jellyfin = {
              icon = "jellyfin.png";
              description = "Home media server";
              href = mkProxyUrl "jellyfin";
              siteMonitor = mkProxyUrl "jellyfin";
              widget = {
                version = 2;
                type = "jellyfin";
                url = mkUrl 8096;
                key = "{{HOMEPAGE_VAR_JELLYFIN_TOKEN}}";
                enableBlocks = true;
                enableNowPlaying = false;
                enableUser = true;
                enableMediaControl = false;
                showEpisodeNumber = true;
              };
            };
          }
          {
            Transmission = {
              icon = "transmission.png";
              description = "Torrent client";
              href = mkProxyUrl "transmission";
              siteMonitor = mkProxyUrl "transmission";
              widget = {
                type = "transmission";
                url = mkUrl config.services.transmission.settings.rpc-port;
                username = "";
                password = "{{HOMEPAGE_VAR_TRANSMISSION_PASSWD}}";
              };
            };
          }
          {
            Vikunja = {
              icon = "vikunja.png";
              description = "Fluffy to-do app";
              href = mkProxyUrl "vikunja";
              siteMonitor = mkProxyUrl "vikunja";
              widget = {
                version = 2;
                type = "vikunja";
                url = mkUrl config.services.vikunja.port;
                key = "{{HOMEPAGE_VAR_VIKUNJA_TOKEN}}";
                enableTaskList = true;
              };
            };
          }
          {
            Mealie = {
              icon = "mealie.png";
              description = "Recipe manager";
              href = mkProxyUrl "mealie";
              siteMonitor = mkProxyUrl "mealie";
              widget = {
                version = 2;
                type = "mealie";
                url = mkUrl config.services.mealie.port;
                key = "{{HOMEPAGE_VAR_MEALIE_TOKEN}}";
              };
            };
          }
          {
            Syncthing = {
              icon = "syncthing.png";
              description = "Device synchronization service";
              href = "http://localhost:8384";
              # widget = {
              #   type = "strelaysrv";
              # };
            };
          }
        ];
      }
      {
        Hosts = [
          {
            ygdrasil = {
              description = "NAS server";
              siteMonitor = mkProxyUrl "glances.ygdrasil";
              href = mkProxyUrl "glances.ygdrasil";
              widget = {
                version = 4;
                type = "glances";
                url = mkProxyUrl "glances.ygdrasil";
                metric = "info";
                refreshInterval = 5000;
              };
            };
          }
          {
            heimdall = {
              description = "DNS server";
              siteMonitor = mkProxyUrl "glances.heimdall";
              href = mkProxyUrl "glances.heimdall";
              widget = {
                version = 4;
                type = "glances";
                url = mkProxyUrl "glances.heimdall";
                metric = "info";
                refreshInterval = 5000;
              };
            };
          }
          {
            phinix = {
              description = "phdenzel's workstation";
              siteMonitor = mkProxyUrl "glances.phinix";
              href = mkProxyUrl "glances.phinix";
              widget = {
                version = 4;
                type = "glances";
                url = mkProxyUrl "glances.phinix";
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
