{...}: {
  networking.firewall.allowedTCPPorts = [53];
  networking.firewall.allowedUDPPorts = [53];
  services.blocky = {
    enable = true;
    settings = {
      ports = {
        dns = 53;
        # tls = 853;
        https = 443;
        http = 4000;
      };
      upstreams = {
        init.strategy = "fast"; # "blocking";
        groups.default = [
          "tcp-tls:dns.digitale-gesellschaft.ch:853"
          "tcp-tls:fdns1.dismail.de:853"
          "tcp-tls:dns3.digitalcourage.de:853"
          "9.9.9.9"
          "1.1.1.1"
        ];
        strategy = "parallel_best";
        timeout = "2s";
      };
      connectIPVersion = "v4"; # v4
      customDNS = {
        rewrite = {
          "local" = "home";
          "lan" = "home";
        };
        mapping = {
          # Machines
          "fritzbox.home" = "192.168.178.1";
          "heimdall.home" = "192.168.178.64";
          "ygdrasil.home" = "192.168.178.42";
          "phinix.home" = "192.168.178.156";
          "ipmi-phinix.home" = "192.168.178.157";
          "fenrix.home" = "192.168.178.138,192.168.178.140";
          "asahi.home" = "192.168.178.138,192.168.178.139";
          # Services
          "jellyfin.home" = "192.168.178.42";
          "nextcloud.home" = "192.168.178.42";
          "homematic.home" = "192.168.178.41";
        };
      };
      conditional = {
        fallbackUpstream = false;
        mapping = {
          "fritz.box" = "192.168.178.1";
          "178.168.192.in-addr.arpa" = "192.168.178.1";
          # "." = "168.168.0.1";
        };
      };
      # clientLookup.upstream = "192.168.178.1";
      blocking = {
        denylists = {
          ads = [
            "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts"
            "https://adaway.org/hosts.txt"
            "https://v.firebog.net/hosts/AdguardDNS.txt"
            "https://v.firebog.net/hosts/Admiral.txt"
            "https://raw.githubusercontent.com/anudeepND/blacklist/master/adservers.txt"
            "https://v.firebog.net/hosts/Easylist.txt"
            "https://pgl.yoyo.org/adservers/serverlist.php?hostformat=hosts&showintro=0&mimetype=plaintext"
            "https://raw.githubusercontent.com/FadeMind/hosts.extras/master/UncheckyAds/hosts"
            "https://raw.githubusercontent.com/bigdargon/hostsVN/master/hosts"
            "https://blocklistproject.github.io/Lists/ads.txt"
            "https://blocklistproject.github.io/Lists/gambling.txt"
          ];
          adult = [
            "https://raw.githubusercontent.com/chadmayfield/my-pihole-blocklists/master/lists/pi_blocklist_porn_top1m.list"
            "https://v.firebog.net/hosts/Prigent-Adult.txt"
            # contains errors: "https://blocklistproject.github.io/Lists/porn.txt"
          ];
          malware = [
            "https://urlhaus.abuse.ch/downloads/hostfile"
            # contains errors: "https://raw.githubusercontent.com/DandelionSprout/adfilt/master/Alternate%20versions%20Anti-Malware%20List/AntiMalwareHosts.txt"
            "https://v.firebog.net/hosts/Prigent-Crypto.txt"
            "https://raw.githubusercontent.com/FadeMind/hosts.extras/master/add.Risk/hosts"
            "https://bitbucket.org/ethanr/dns-blacklists/raw/8575c9f96e5b4a1308f2f12394abd86d0927a4a0/bad_lists/Mandiant_APT1_Report_Appendix_D.txt"
            "https://phishing.army/download/phishing_army_blocklist_extended.txt"
            "https://gitlab.com/quidsup/notrack-blocklists/raw/master/notrack-malware.txt"
            # contains errors: "https://v.firebog.net/hosts/RPiList-Malware.txt"
            "https://raw.githubusercontent.com/Spam404/lists/master/main-blacklist.txt"
            "https://raw.githubusercontent.com/AssoEchap/stalkerware-indicators/master/generated/hosts"
            "https://blocklistproject.github.io/Lists/malware.txt"
            "https://blocklistproject.github.io/Lists/abuse.txt"
            "https://blocklistproject.github.io/Lists/fraud.txt"
            "https://blocklistproject.github.io/Lists/phishing.txt"
            "https://blocklistproject.github.io/Lists/ransomware.txt"
            "https://blocklistproject.github.io/Lists/scam.txt"
            # contains errors: "https://lists.cyberhost.uk/malware.txt"
          ];
          trackers = [
            "https://v.firebog.net/hosts/Easyprivacy.txt"
            "https://v.firebog.net/hosts/Prigent-Ads.txt"
            "https://raw.githubusercontent.com/FadeMind/hosts.extras/master/add.2o7Net/hosts"
            "https://raw.githubusercontent.com/crazy-max/WindowsSpyBlocker/master/data/hosts/spy.txt"
            "https://hostfiles.frogeye.fr/firstparty-trackers-hosts.txt"
            "https://blocklistproject.github.io/Lists/tracking.txt"
            "https://blocklistproject.github.io/Lists/smart-tv.txt"
          ];
          special = [
            "https://raw.githubusercontent.com/StevenBlack/hosts/master/alternates/fakenews/hosts"
            "https://raw.githubusercontent.com/PolishFiltersTeam/KADhosts/master/KADhosts.txt"
            "https://raw.githubusercontent.com/FadeMind/hosts.extras/master/add.Spam/hosts"
            "https://v.firebog.net/hosts/static/w3kbl.txt"
            "https://blocklistproject.github.io/Lists/redirect.txt"
          ];
          restrictive = [
            # Social media
            "https://raw.githubusercontent.com/anudeepND/blacklist/master/facebook.txt"
            # Trackers
            "https://raw.githubusercontent.com/Perflyst/PiHoleBlocklist/master/android-tracking.txt"
            "https://raw.githubusercontent.com/Perflyst/PiHoleBlocklist/master/SmartTV.txt"
            # Malware
            "https://v.firebog.net/hosts/Prigent-Malware.txt"
            # contains errors: "https://v.firebog.net/hosts/RPiList-Phishing.txt"
            
          ];
        };
        # allowlists = {};
        clientGroupsBlock = {
          default = ["ads" "malware" "trackers" "special"];
        };
        blockType = "nxDomain";
        blockTTL = "1h";
        loading = {
          refreshPeriod = "24h";
          downloads = {
            timeout = "5m";
            writeTimeout = "5m";
            readTimeout = "3m";
            readHeaderTimeout = "60s";
            cooldown = "30s";
            attempts = 5;
          };
          concurrency = 4;
          strategy = "failOnError";
          maxErrorsPerSource = 8;
        };
      };
      caching = {
        minTime = "5m";
        maxTime = "30m";
        maxItemsCount = 0;
        prefetching = true;
        prefetchExpires = "6h";
        prefetchThreshold = 5;
        prefetchMaxItemsCount = 0;
        cacheTimeNegative = "30m";
      };
      log = {
        level = "info";
        format = "text";
        timestamp = true;
        privacy = false;
      };
      prometheus = {
        enable = true;
        path = "/metrics";
      };
    };
  };
}
