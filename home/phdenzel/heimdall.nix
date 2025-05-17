{
  pkgs,
  config,
  inputs,
  ...
}: let
  ifTheyExist = groups: builtins.filter (group: builtins.hasAttr group config.users.groups) groups;
  userName = "phdenzel";
  hostName = "heimdall";
in {
  imports = [
    inputs.sops-nix.nixosModules.sops
    ../_common/sops.nix
  ];

  sops-user = {
    enable = true;
    user = "${userName}";
    host = "${hostName}";
    sshKeys = ["id_ed25519"];
  };

  users.users.${userName} = {
    isNormalUser = true;
    shell = pkgs.bash;
    extraGroups =
      [
        "audio"
        "video"
        "wheel"
      ]
      ++ ifTheyExist [
        "disk"
        "git"
        "input"
        "kvm"
        "libvirtd"
        "network"
        "networkmanager"
        "podman"
        "storage"
      ];
    hashedPasswordFile = config.sops.secrets."passwd/${userName}/${hostName}".path;
    # packages = with pkgs; [
    #   ...
    # ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDLoBbnz9XBvuq7QIUT1cPpyn32PWJFEnH1tPJAidJvO phdenzel@phinix"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOr6HdrDEH1BztobKQo9xZqlqqYUEWTuz5+QricptROm phdenzel@fenrix"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGZofJltIURsWCGEc+H5wyp4WJ3GGkcjPR5THptcR1dg phdenzel@asahi"
    ];
  };
}
