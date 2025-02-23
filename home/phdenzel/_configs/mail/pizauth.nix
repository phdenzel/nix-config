{pkgs, config, lib, ...}: let
  pizAuthConf = "pizauth.conf";
  username = config.home.username;
  usernameAlt = "denp";
in {
  home.file."${config.xdg.configHome}/${pizAuthConf}".text = ''
    account "ms" {
      auth_uri = "https://login.microsoftonline.com/common/oauth2/v2.0/authorize";
      token_uri = "https://login.microsoftonline.com/common/oauth2/v2.0/token";
      client_id = "9e5f94bc-e8a4-4e73-b8be-63364c29d753";
      redirect_uri = "https://localhost";
      scopes = [
        "https://outlook.office.com/IMAP.AccessAsUser.All",
        "https://outlook.office.com/SMTP.Send",
        "offline_access"
      ];
      auth_uri_fields = {"login_hint": "${username}@hotmail.com"};
    }

    account "zhaw" {
      auth_uri = "https://login.microsoftonline.com/common/oauth2/v2.0/authorize";
      token_uri = "https://login.microsoftonline.com/common/oauth2/v2.0/token";
      client_id = "9e5f94bc-e8a4-4e73-b8be-63364c29d753";
      redirect_uri = "https://localhost";
      scopes = [
        "https://outlook.office.com/IMAP.AccessAsUser.All",
        "https://outlook.office.com/SMTP.Send",
        "offline_access"
      ];
      auth_uri_fields = {"login_hint": "${usernameAlt}@zhaw.ch"};
    }'';

  systemd.user = {
    services = {
      pizauth = {
        Unit = {
          Description = "Pizauth OAuth token manager.";
        };
        Service = {
          ExecStart = "${lib.getExe pkgs.pizauth} server -d";
          ExecReload = "${lib.getExe pkgs.pizauth} reload";
          ExecStop = "${lib.getExe pkgs.pizauth} shutdown";
        };
        Install = {
          WantedBy = ["default.target"];
        };
      };
    };
    services = {
      pizauth-restore = {
        Unit = {
          Description = "Pizauth OAuth token manager cache restore.";
        };
        Service = {
          After = "pizauth.service";
          ExecStart = "${pkgs.writeShellScript "pizauth-on-launch" ''
            #!/run/current-system/sw/bin/bash
            [ -f "${config.xdg.configHome}/pizauth.age" ] && [ -r "${config.xdg.configHome}/sops/age/keys.txt" ] && ${lib.getExe pkgs.age} --decrypt -i ${config.xdg.configHome}/sops/age/keys.txt -o - ${config.xdg.configHome}/pizauth.age | ${lib.getExe pkgs.pizauth} restore
            ''}";
        };
        Install = {
          WantedBy = ["default.target"];
        };
      };
    };
  };
}
