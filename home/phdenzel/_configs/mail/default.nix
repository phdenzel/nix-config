{pkgs, lib, config, inputs, ...}: let
  capitalize = str: (lib.strings.concatStrings [
    (lib.strings.toUpper (lib.strings.substring 0 1 str))
    (lib.strings.substring 1 (builtins.stringLength str) str)
  ]);
  username = config.home.username;
  usernameAlt = "denp";
  surname = "denzel";
  personalname = "philipp";
  realname = "${lib.strings.concatStringsSep " " [(capitalize personalname) (capitalize surname)]}";
  casualSignature = "-PhD";
  workSignature = "${realname}\nCentre for Artificial Intelligence CAI\nZurich University of Applied Science ZHAW\nTechnikumstrasse 71, CH-8400 Winterthur\nT: +41 (0) 58 934 78 23\nE: ${personalname}.${surname}@zhaw.ch\nW: https://www.zhaw.ch/en/about-us/person/${usernameAlt}";
in {
  programs.mbsync.enable = true;
  
  accounts.email = {
    maildirBasePath = "Mail";
    accounts = {
      gmail = rec {
        primary = true;
        flavor = "gmail.com";
        realName = "${realname}";
        address = "${username}@${flavor}";
        userName = "${address}";
        passwordCommand = "${lib.getExe pkgs.pass} mail/${address}";
        signature = {
          showSignature = "append";
          delimiter = "   ";
          text = "${casualSignature}";
        };
        folders = {
          inbox = "Inbox";
          sent = "\[Gmail\]/Sent Mail";
          drafts = "\[Gmail\]/Drafts";
          trash = "\[Gmail\]/Trash";
        };
        imap = {
          host = "imap.${flavor}";
          port = 993;
          tls.enable = true;
        };
        smtp = {
          host = "smtp.${flavor}";
          # port = 587;
          tls.enable = true;
        };
        mu.enable = true;
        mbsync = {
          enable = true;
          create = "both";
          expunge = "both";
          subFolders = "Verbatim";
          patterns = ["*" "![Gmail]*" "[Gmail]/Sent Mail" "[Gmail]/Starred" "[Gmail]/All Mail" "[Gmail]/Trash" "[Gmail]/Spam"];
        };
        thunderbird = {
          enable = true;
          # perIdentitySettings = _: {};
          # settings = _: {};
        };
      };

      # <name> = rec {
      #   primary = true;
      #   flavor = "plain"; # “gmail.com”, “fastmail.com”, “outlook.office365.com”
      #   realName = "${realname}";
      #   address = "${username}@${flavor}";
      #   userName = "${address}";
      #   passwordCommand = "${lib.getExe pkgs.pass} <name>";
      #   maildir.path = "\${name}";
      #   signature = {
      #     showSignature = "append";
      #     text = "${personalSignature}";
      #   };
      #   folders = {
      #     drafts = "Drafts";
      #     inbox = "Inbox";
      #     sent = "Sent";
      #     trash = "Trash";
      #   };
      #   imap = {
      #     host = "imap.example.org";
      #     port = 993;
      #     tls = {
      #       enable = true;
      #       useStartTls = false;
      #     };
      #   };
      #   smtp = {
      #     host = "smtp.example.org";
      #     port = 465;
      #     tls = {
      #       enable = true;
      #       tls.useStartTls = false;
      #     };
      #   };
      #   mu.enable = true;
      #   mbsync = {
      #     enable = true;
      #     create = "none";
      #     expunge = "none";
      #     extraConfig = {
      #       account = {};
      #       channel = {};
      #       local = {};
      #       remote = {};
      #     };
      #     flatten = null;
      #     groups = {};
      #   };
      #   thunderbird = {
      #     enable = true;
      #     perIdentitySettings = _: {};
      #     settings = _: {};
      #   };
      # };

    };
  };
  
  
}
