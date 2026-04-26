{
  pkgs,
  config,
  lib,
  inputs,
  ...
}: let
  background_image = "homepage-dashboard/assets/background.png";
  mkProxyUrl = service: "http://${service}.home";
  mkProxyUrlHttps = service: "https://${service}.home";
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
      lib.strings.concatStringsSep "," (
        [
          "localhost"
          "127.0.0.1"
          "127.0.0.1:9200"
          "syncthing.ygdrasil.home/rest/system/connections"
          # "syncthing.ygdrasil.home/rest/system/status"
        ] ++ map (s: "${s}.home") [
          "${config.networking.hostName}"
          "denzels"
          "traefik"
          "syncthing.ygdrasil"
          "opencloud"
          "forgejo"
          "jellyfin"
          "transmission"
          "vikunja"
          "mealie"
        ]
      );
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
                url = mkProxyUrl "traefik";
              };
            };
          }
          {
            Syncthing = {
              icon = "syncthing.png";
              description = "Device synchronization service";
              href = mkProxyUrl "syncthing.ygdrasil";
              siteMonitor = mkProxyUrl "syncthing.ygdrasil";
              widget = {
                type = "customapi";
                url = "${mkProxyUrl "syncthing.ygdrasil"}/rest/system/connections";
                # url = "${mkProxyUrl "syncthing.ygdrasil"}/rest/system/status";
                headers = {
                  X-API-Key = "{{HOMEPAGE_VAR_SYNCTHING_TOKEN}}";
                };
                mappings = [
                  { field = "total.inBytesTotal"; label = "In"; format = "float"; scale = "0.000001"; suffix = " MB"; }
                  { field = "total.outBytesTotal"; label = "Out"; format = "float"; scale = "0.000001"; suffix = " MB"; }
                  { field = "total.at"; label = "Updated"; format = "relativeDate"; }
                  # { field = "uptime"; label = "Uptime (h)"; format = "float"; scale = "0.000277778"; suffix = "h"; }
                  # { field = "cpuPercent"; label = "CPU"; format = "float"; suffix = "%"; }
                  # { field = "alloc"; label = "Memory"; format = "bytes"; }
                ];
              };
            };
          }
          {
            Opencloud = {
              icon = "sh-opencloud.png";
              description = "Private cloud service";
              href = mkProxyUrlHttps "opencloud";
              siteMonitor = mkProxyUrlHttps "opencloud";
              widget = {
                type = "customapi";
                url = "${mkProxyUrlHttps "opencloud"}/graph/v1.0/drives?$filter=driveType+eq+'project'&$orderby=name+asc";
                allowInsecure = true;
                headers = {
                  Authorization = "Basic {{HOMEPAGE_VAR_OPENCLOUD_TOKEN}}";
                };
                mappings = [
                  {
                    field = { value = { "0" = { quota = "used"; }; }; };
                    label = "Backups";
                    format = "bytes";
                  }
                  {
                    field = { value = { "1" = { quota = "used"; }; }; };
                    label = "Media";
                    format = "bytes";
                  }
                  {
                    field = { value = { "2" = { quota = "used"; }; }; };
                    label = "Shared";
                    format = "bytes";
                  }
                ];
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
                url = mkProxyUrl "forgejo";
                key = "{{HOMEPAGE_VAR_FORGEJO_TOKEN}}";
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
                url = mkProxyUrl "jellyfin";
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
                url = mkProxyUrl "transmission";
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
                url = mkProxyUrl "vikunja";
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
                url = mkProxyUrl "mealie";
                key = "{{HOMEPAGE_VAR_MEALIE_TOKEN}}";
              };
            };
          }
        ];
      }
      {
        Hosts = [
          {
            ygdrasil = {
              description = "NAS server";
              icon = "glances.png";
              href = mkProxyUrl "glances.ygdrasil";
              siteMonitor = mkProxyUrl "glances.ygdrasil";
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
              icon = "glances.png";
              href = mkProxyUrl "glances.heimdall";
              siteMonitor = mkProxyUrl "glances.heimdall";
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
              icon = "glances.png";
              href = mkProxyUrl "glances.phinix";
              siteMonitor = mkProxyUrl "glances.phinix";
              widget = {
                version = 4;
                type = "glances";
                url = mkProxyUrl "glances.phinix";
                metric = "info";
                refreshInterval = 5000;
              };
            };
          }
          {
            sol = {
              description = "phdenzel's NUC";
              icon = "glances.png";
              href = mkProxyUrl "glances.sol";
              siteMonitor = mkProxyUrl "glances.sol";
              widget = {
                version = 4;
                type = "glances";
                url = mkProxyUrl "glances.sol";
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
