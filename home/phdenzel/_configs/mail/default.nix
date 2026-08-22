{
  pkgs,
  lib,
  config,
  ...
}: let
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
  defaultMbsyncConfig = p: {
    enable = true;
    create = "both";
    expunge = "both";
    subFolders = "Verbatim";
    patterns = if (lib.hasAttr "patterns" p) then p.patterns else ["*"];
    extraConfig.account = lib.mkIf (lib.hasAttr "accountExtra" p) p.accountExtra;
  };
in {
  imports = [./pizauth.nix];

  # TODO: migrate to a more modern IMAP synchronizer
  #   -> neverest
  programs.mbsync = {
    enable = true;
    package = pkgs.isync.override {
      withCyrusSaslXoauth2 = true;
    };
  };

  programs.himalaya = {
    enable = true;
    package = pkgs.himalaya;
    settings = {
      downloads-dir = "${config.home.homeDirectory}/Downloads";
    };
  };

  accounts.email = {
    maildirBasePath = "Mail";
    accounts = {
      proton = rec {
        flavor = "plain";
        realName = "${realname}";
        address = "${username}@protonmail.com";
        userName = "${address}";
        # ProtonMail Bridge runs a local IMAP/SMTP gateway that authenticates
        # with a bridge-generated password (not the account password).
        # Store that password once via: pass insert mail/${username}@protonmail-bridge
        #
        # The Bridge serves a self-signed cert (CN=127.0.0.1, SAN 127.0.0.1,
        # Proton AG) over STARTTLS, regenerated per host, so it is trusted
        # manually rather than committed. Just Get Messages / send a mail and
        # accept the in-context cert pop-up, once for IMAP 1143 and once for
        # SMTP 1025 (each port stores its own override in the TB profile).
        # Redo on any new host, or whenever the Bridge regenerates its cert.
        passwordCommand = "${lib.getExe pkgs.pass} mail/${username}@protonmail-bridge";
        signature = {
          showSignature = "append";
          delimiter = "   ";
          text = "${casualSignature}";
        };
        folders = {
          inbox = "INBOX";
          sent = "Sent";
          drafts = "Drafts";
          trash = "Trash";
        };
        imap = {
          host = "127.0.0.1";
          port = 1143;
          tls.enable = true;
          tls.useStartTls = true;
        };
        smtp = {
          host = "127.0.0.1";
          port = lib.mkForce 1025;
          tls.enable = true;
          tls.useStartTls = true;
        };
        thunderbird = {
          enable = true;
          settings = id: {
            "mail.identity.id_${id}.reply_on_top" = 1;
            "mail.identity.id_${id}.sig_bottom" = false;
          };
        };
      };

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
          tls.enable = true;
        };
        mu.enable = true;
        mbsync = defaultMbsyncConfig {
          patterns = ["*" "![Gmail]*" "[Gmail]/Sent Mail" "[Gmail]/Starred" "[Gmail]/All Mail" "[Gmail]/Trash" "[Gmail]/Spam"];
          accountExtra = {AuthMechs = "PLAIN";};
        };
        himalaya = {
          enable = true;
          settings = {
            message.send.save-copy = true;
            sync.enable = true;
          };
        };
        thunderbird = {
          enable = true;
          settings = id: {
            "mail.identity.id_${id}.reply_on_top" = 1;
            "mail.identity.id_${id}.sig_bottom" = false;
          };
        };
      };

      ikmail = rec {
        flavor = "plain";
        realName = "${realname}";
        address = "${username}@ikmail.com";
        userName = "${address}";
        passwordCommand = "${lib.getExe pkgs.pass} mail/${address}";
        signature = {
          showSignature = "append";
          delimiter = "   ";
          text = "${casualSignature}";
        };
        folders = {
          inbox = "Inbox";
          sent = "Sent";
          drafts = "Drafts";
          trash = "Trash";
        };
        imap = {
          host = "mail.infomaniak.com";
          port = 993;
          tls.enable = true;
        };
        smtp = {
          host = "mail.infomaniak.com";
          port = lib.mkForce 465;
          tls.enable = true;
        };
        mu.enable = true;
        mbsync = defaultMbsyncConfig {
          accountExtra = {AuthMechs = "PLAIN";};
        };
        himalaya = {
          enable = true;
          settings = {
            message.send.save-copy = true;
            sync.enable = true;
          };
        };
        thunderbird = {
          enable = true;
          settings = id: {
            "mail.identity.id_${id}.reply_on_top" = 1;
            "mail.identity.id_${id}.sig_bottom" = false;
          };
        };
      };

      hispeed = rec {
        flavor = "plain";
        realName = "${realname}";
        address = "${username}@hispeed.ch";
        userName = "${address}";
        passwordCommand = "${lib.getExe pkgs.pass} mail/${address}";
        signature = {
          showSignature = "append";
          delimiter = "   ";
          text = "${casualSignature}";
        };
        folders = {
          drafts = "Drafts";
          inbox = "Inbox";
          sent = "SentMail";
          trash = "Trash";
        };
        imap = {
          host = "imap.hispeed.ch";
          port = 993;
          tls.enable = true;
        };
        smtp = {
          host = "smtp.hispeed.ch";
          port = lib.mkForce 465;
          tls.enable = true;
        };
        mu.enable = true;
        mbsync = defaultMbsyncConfig {
          patterns = ["*" "!Sent"];
          accountExtra = {AuthMechs = "PLAIN";};
        };
        himalaya = {
          enable = true;
          settings = {
            message.send.save-copy = true;
            sync.enable = true;
          };
        };
        thunderbird = {
          enable = true;
          settings = id: {
            "mail.identity.id_${id}.reply_on_top" = 1;
            "mail.identity.id_${id}.sig_bottom" = false;
          };
        };
      };

      icloud = rec {
        flavor = "plain";
        realName = "${realname}";
        address = "${username}@icloud.com";
        userName = "${address}";
        passwordCommand = "${lib.getExe pkgs.pass} mail/${address}";
        signature = {
          showSignature = "append";
          delimiter = "   ";
          text = "${casualSignature}";
        };
        folders = {
          drafts = "Drafts";
          inbox = "Inbox";
          sent = "Sent";
          trash = "Trash";
        };
        imap = {
          host = "imap.mail.me.com";
          port = 993;
          tls.enable = true;
        };
        smtp = {
          host = "smtp.mail.me.com";
          port = lib.mkForce 465;
          tls.enable = true;
        };
        mu.enable = true;
        mbsync = defaultMbsyncConfig {
          accountExtra = {AuthMechs = "PLAIN";};
        };
        himalaya = {
          enable = true;
          settings = {
            message.send.save-copy = true;
            sync.enable = true;
          };
        };
        thunderbird = {
          enable = true;
          settings = id: {
            "mail.identity.id_${id}.reply_on_top" = 1;
            "mail.identity.id_${id}.sig_bottom" = false;
          };
        };
      };

      ms = rec {
        flavor = "outlook.office365.com";
        realName = "${realname}";
        address = "${username}@hotmail.com";
        userName = "${address}";
        passwordCommand = "${lib.getExe pkgs.pizauth} show ms";
        signature = {
          showSignature = "append";
          delimiter = "   ";
          text = "${casualSignature}";
        };
        folders = {
          drafts = "Drafts";
          inbox = "Inbox";
          sent = "Sent";
          trash = "Trash";
        };
        imap = {
          host = "${flavor}";
          port = 993;
          tls.enable = true;
          tls.useStartTls = false;
        };
        smtp = {
          host = "smtp.office365.com";
          tls.enable = true;
          tls.useStartTls = true;
        };
        mu.enable = true;
        mbsync = defaultMbsyncConfig {
          patterns = ["*" "!Notes"];
          accountExtra = {AuthMechs = "XOAUTH2";};
        };

        himalaya = {
          enable = true;
          settings = {
            message.send.save-copy = true;
            sync.enable = true;
            backend.auth = {
              type = "oauth2";
              method = "xoauth2";
              client-id = "9e5f94bc-e8a4-4e73-b8be-63364c29d753";
              access-token.cmd = "pizauth show ms";
              refresh-toke.cmd = "pizauth refresh ms";
              auth-url = "https://login.microsoftonline.com/7800fe5b-373f-499a-8767-f183879091cd/oauth2/v2.0/authorize";
              token-url = "https://login.microsoftonline.com/7800fe5b-373f-499a-8767-f183879091cd/oauth2/v2.0/token";
              pkce = true;
              scopes = ["https://graph.microsoft.com/IMAP.AccessAsUser.All" "https://graph.microsoft.com/SMTP.Send"];
            };            
            message.send.backend.auth = {
              type = "oauth2";
              method = "xoauth2";
              client-id = "9e5f94bc-e8a4-4e73-b8be-63364c29d753";
              access-token.cmd = "pizauth show ms";
              refresh-toke.cmd = "pizauth refresh ms";
              auth-url = "https://login.microsoftonline.com/common/oauth2/v2.0/authorize";
              token-url = "https://login.microsoftonline.com/common/oauth2/v2.0/token";
              pkce = true;
              scopes = ["https://outlook.office.com/IMAP.AccessAsUser.All" "https://outlook.office.com/SMTP.Send"];
            };
          };
        };
        thunderbird = {
          enable = true;
          settings = id: {
            "mail.identity.id_${id}.reply_on_top" = 1;
            "mail.identity.id_${id}.sig_bottom" = false;
            "mail.server.server_${id}.authMethod" = 10;
            "mail.smtpserver.smtp_${id}.authMethod" = 10;
          };
        };
      };

      zhaw = rec {
        flavor = "outlook.office365.com";
        realName = "${realname}";
        address = "${usernameAlt}@zhaw.ch";
        userName = "${address}";
        passwordCommand = "${lib.getExe pkgs.pizauth} show zhaw";
        signature = {
          showSignature = "append";
          delimiter = "--";
          text = "${workSignature}";
        };
        folders = {
          drafts = "Drafts";
          inbox = "Inbox";
          sent = "Sent Items";
          trash = "Deleted Items";
        };
        imap = {
          host = "${flavor}";
          port = 993;
          tls.enable = true;
        };
        smtp = {
          host = "smtp.office365.com";
          tls.enable = true;
          tls.useStartTls = true;
        };
        mu.enable = true;
        mbsync = defaultMbsyncConfig {
          patterns = ["Inbox" "Archive" "Sent Items" "Drafts" "Deleted Items" "Junk Email"];
          accountExtra = {AuthMechs = "XOAUTH2";};
        };
        himalaya = {
          enable = true;
          settings = {
            message.send.save-copy = true;
            sync.enable = true;
            backend.auth = {
              type = "oauth2";
              method = "xoauth2";
              client-id = "9e5f94bc-e8a4-4e73-b8be-63364c29d753";
              access-token.cmd = "pizauth show zhaw";
              refresh-toke.cmd = "pizauth refresh zhaw";
              # client-secret.keyring = "outlook-oauth2-client-secret";
              # access-token.keyring = "outlook-oauth2-access-token";
              # refresh-token.keyring = "outlook-oauth2-refresh-token";
              auth-url = "https://login.microsoftonline.com/common/oauth2/v2.0/authorize";
              token-url = "https://login.microsoftonline.com/common/oauth2/v2.0/token";
              pkce = true;
              scopes = ["https://outlook.office.com/IMAP.AccessAsUser.All" "https://outlook.office.com/SMTP.Send"];
            };
            message.send.backend.auth = {
              type = "oauth2";
              method = "xoauth2";
              client-id = "9e5f94bc-e8a4-4e73-b8be-63364c29d753";
              access-token.cmd = "pizauth show zhaw";
              refresh-toke.cmd = "pizauth refresh zhaw";
              # client-secret.keyring = "outlook-oauth2-client-secret";
              # access-token.keyring = "outlook-oauth2-access-token";
              # refresh-token.keyring = "outlook-oauth2-refresh-token";
              auth-url = "https://login.microsoftonline.com/common/oauth2/v2.0/authorize";
              token-url = "https://login.microsoftonline.com/common/oauth2/v2.0/token";
              pkce = true;
              scopes = ["https://outlook.office.com/IMAP.AccessAsUser.All" "https://outlook.office.com/SMTP.Send"];
            };
          };
        };
        thunderbird = {
          enable = true;
          settings = id: {
            "mail.identity.id_${id}.reply_on_top" = 1;
            "mail.identity.id_${id}.sig_bottom" = false;
            "mail.server.server_${id}.authMethod" = 10;
            "mail.smtpserver.smtp_${id}.authMethod" = 10;
          };
        };
      };
    };
  };
}
