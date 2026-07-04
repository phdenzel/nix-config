{...}: {
  programs.ssh.settings = {
    # Git platforms
    "gitlab.com" = {
      HostName = "gitlab.com";
      User = "git";
      IdentityFile = "~/.ssh/gl_id_ed25519";
      Compression = false;
      ForwardAgent = true;
      AddKeysToAgent = "yes";
    };
    "github.com" = {
      HostName = "github.com";
      User = "git";
      IdentityFile = "~/.ssh/gh_id_ed25519";
      Compression = false;
      ForwardAgent = true;
      AddKeysToAgent = "yes";
    };
    "github.zhaw.ch" = {
      HostName = "github.zhaw.ch";
      User = "git";
      IdentityFile = "~/.ssh/ghzhaw_id_ed25519";
      Compression = false;
      ForwardAgent = true;
      AddKeysToAgent = "yes";
    };

    # Home environment
    "idun" = {
      HostName = "idun.home";
      User = "phdenzel";
      Port = 22220;
      IdentityFile = "~/.ssh/id_ed25519";
      Compression = false;
      ForwardAgent = true;
      AddKeysToAgent = "yes";
    };
    "ygdrasil" = {
      HostName = "ygdrasil.home";
      User = "phdenzel";
      Port = 22;
      IdentityFile = "~/.ssh/id_ed25519";
      Compression = false;
      ForwardAgent = true;
      AddKeysToAgent = "yes";
    };
    "heimdall" = {
      HostName = "heimdall.home";
      User = "phdenzel";
      Port = 22;
      IdentityFile = "~/.ssh/id_ed25519";
      Compression = false;
      ForwardAgent = true;
      AddKeysToAgent = "yes";
    };
    "loki" = {
      HostName = "loki.home";
      User = "phdenzel";
      Port = 22;
      IdentityFile = "~/.ssh/id_ed25519";
      Compression = false;
      ForwardAgent = true;
      AddKeysToAgent = "yes";
    };
    "hugin" = {
      HostName = "hugin.home";
      User = "phdenzel";
      Port = 22;
      IdentityFile = "~/.ssh/id_ed25519";
      Compression = false;
      ForwardAgent = true;
      AddKeysToAgent = "yes";
    };
    "munin" = {
      HostName = "munin.home";
      User = "phdenzel";
      Port = 22;
      IdentityFile = "~/.ssh/id_ed25519";
      Compression = false;
      ForwardAgent = true;
      AddKeysToAgent = "yes";
    };
    "durathror" = {
      HostName = "durathror.home";
      User = "phdenzel";
      Port = 22;
      IdentityFile = "~/.ssh/id_ed25519";
      Compression = false;
      ForwardAgent = true;
      AddKeysToAgent = "yes";
    };
    "dvalar" = {
      HostName = "dvalar.home";
      User = "phdenzel";
      Port = 22;
      IdentityFile = "~/.ssh/id_ed25519";
      Compression = false;
      ForwardAgent = true;
      AddKeysToAgent = "yes";
    };
    "duneyr" = {
      HostName = "dain.home";
      User = "phdenzel";
      Port = 22;
      IdentityFile = "~/.ssh/id_ed25519";
      Compression = false;
      ForwardAgent = true;
      AddKeysToAgent = "yes";
    };
    "phinix" = {
      HostName = "phinix.home";
      User = "phdenzel";
      Port = 22;
      IdentityFile = "~/.ssh/id_ed25519";
      Compression = false;
      ForwardAgent = true;
      AddKeysToAgent = "yes";
    };
    "sol" = {
      HostName = "sol.home";
      User = "phdenzel";
      Port = 22;
      IdentityFile = "~/.ssh/id_ed25519";
      Compression = false;
      ForwardAgent = true;
      AddKeysToAgent = "yes";
    };
    "fenrix" = {
      HostName = "fenrix.home";
      User = "phdenzel";
      Port = 22;
      IdentityFile = "~/.ssh/id_ed25519";
      Compression = false;
      ForwardAgent = true;
      AddKeysToAgent = "yes";
    };
    "asahi" = {
      HostName = "asahi.home";
      User = "phdenzel";
      Port = 22;
      IdentityFile = "~/.ssh/id_ed25519";
      Compression = false;
      ForwardAgent = true;
      AddKeysToAgent = "yes";
    };

    # Research clusters
    "ela" = {
      HostName = "ela.cscs.ch";
      User = "pdenzel";
      IdentityFile = "~/.ssh/cscs_signed_key";
      Compression = false;
      ForwardAgent = true;
      AddKeysToAgent = "yes";
    };
    "daint" = {
      HostName = "daint.alps.cscs.ch";
      User = "pdenzel";
      IdentityFile = "~/.ssh/cscs_signed_key";
      ProxyJump = "ela";
      AddKeysToAgent = "yes";
      IdentitiesOnly = true;
    };
    "eiger" = {
      HostName = "eiger.alps.cscs.ch";
      User = "pdenzel";
      IdentityFile = "~/.ssh/cscs_signed_key";
      ProxyJump = "ela";
      AddKeysToAgent = "yes";
      IdentitiesOnly = true;
    };

    "austin" = {
      HostName = "austin.zhaw.ch";
      User = "denp";
      Port = 22;
      IdentityFile = "~/.ssh/dgx_id_ed25519";
      Compression = false;
      ForwardAgent = true;
      AddKeysToAgent = "yes";
    };
    "dallas" = {
      HostName = "dallas.zhaw.ch";
      User = "denp";
      Port = 22;
      IdentityFile = "~/.ssh/dgx_id_ed25519";
      Compression = false;
      ForwardAgent = true;
      AddKeysToAgent = "yes";
    };
    "losangeles" = {
      HostName = "losangeles.zhaw.ch";
      User = "denp";
      Port = 22;
      IdentityFile = "~/.ssh/dgx_id_ed25519";
      Compression = false;
      ForwardAgent = true;
      AddKeysToAgent = "yes";
    };
    "sanfrancisco" = {
      HostName = "sanfrancisco.zhaw.ch";
      User = "denp";
      Port = 22;
      IdentityFile = "~/.ssh/dgx_id_ed25519";
      Compression = false;
      ForwardAgent = true;
      AddKeysToAgent = "yes";
    };
    "sacramento" = {
      HostName = "sacramento.zhaw.ch";
      User = "denp";
      Port = 22;
      IdentityFile = "~/.ssh/dgx_id_ed25519";
      Compression = false;
      ForwardAgent = true;
      AddKeysToAgent = "yes";
    };
    "sanjose" = {
      HostName = "sanjose.zhaw.ch";
      User = "denp";
      Port = 22;
      IdentityFile = "~/.ssh/dgx_id_ed25519";
      Compression = false;
      ForwardAgent = true;
      AddKeysToAgent = "yes";
    };
    "fresko" = {
      HostName = ".zhaw.ch";
      User = "denp";
      Port = 22;
      IdentityFile = "~/.ssh/dgx_id_ed25519";
      Compression = false;
      ForwardAgent = true;
      AddKeysToAgent = "yes";
    };
    "trinity" = {
      HostName = "trinity.zhaw.ch";
      User = "denp";
      Port = 22;
      IdentityFile = "~/.ssh/dgx_id_ed25519";
      Compression = false;
      ForwardAgent = true;
      AddKeysToAgent = "yes";
    };
    "elpaso" = {
      HostName = "elpaso.zhaw.ch";
      User = "denp";
      Port = 22;
      IdentityFile = "~/.ssh/dgx_id_ed25519";
      Compression = false;
      ForwardAgent = true;
      AddKeysToAgent = "yes";
    };
    "fortworth" = {
      HostName = "fortworth.zhaw.ch";
      User = "denp";
      Port = 22;
      IdentityFile = "~/.ssh/dgx_id_ed25519";
      Compression = false;
      ForwardAgent = true;
      AddKeysToAgent = "yes";
    };
    "lubbock" = {
      HostName = "lubbock.zhaw.ch";
      User = "denp";
      Port = 22;
      IdentityFile = "~/.ssh/dgx_id_ed25519";
      Compression = false;
      ForwardAgent = true;
      AddKeysToAgent = "yes";
    };
  };
}
