{...}: {
  programs.ssh.matchBlocks = {
    # Git platforms
    "gitlab.com" = {
      hostname = "gitlab.com";
      user = "git";
      identityFile = "~/.ssh/gl_id_ed25519";
      compression = false;
      forwardAgent = true;
      addKeysToAgent = "yes";
    };
    "github.com" = {
      hostname = "github.com";
      user = "git";
      identityFile = "~/.ssh/gh_id_ed25519";
      compression = false;
      forwardAgent = true;
      addKeysToAgent = "yes";
    };
    "github.zhaw.ch" = {
      hostname = "github.zhaw.ch";
      user = "git";
      identityFile = "~/.ssh/ghzhaw_id_ed25519";
      compression = false;
      forwardAgent = true;
      addKeysToAgent = "yes";
    };

    # Home environment
    "idun" = {
      hostname = "idun.home";
      user = "phdenzel";
      port = 22220;
      identityFile = "~/.ssh/id_ed25519";
      compression = false;
      forwardAgent = true;
      addKeysToAgent = "yes";
    };
    "ygdrasil" = {
      hostname = "ygdrasil.home";
      user = "phdenzel";
      port = 22;
      identityFile = "~/.ssh/id_ed25519";
      compression = false;
      forwardAgent = true;
      addKeysToAgent = "yes";
    };
    "heimdall" = {
      hostname = "heimdall.home";
      user = "phdenzel";
      port = 22;
      identityFile = "~/.ssh/id_ed25519";
      compression = false;
      forwardAgent = true;
      addKeysToAgent = "yes";
    };
    "loki" = {
      hostname = "loki.home";
      user = "phdenzel";
      port = 22;
      identityFile = "~/.ssh/id_ed25519";
      compression = false;
      forwardAgent = true;
      addKeysToAgent = "yes";
    };
    "hugin" = {
      hostname = "hugin.home";
      user = "phdenzel";
      port = 22;
      identityFile = "~/.ssh/id_ed25519";
      compression = false;
      forwardAgent = true;
      addKeysToAgent = "yes";
    };
    "munin" = {
      hostname = "munin.home";
      user = "phdenzel";
      port = 22;
      identityFile = "~/.ssh/id_ed25519";
      compression = false;
      forwardAgent = true;
      addKeysToAgent = "yes";
    };
    "durathror" = {
      hostname = "durathror.home";
      user = "phdenzel";
      port = 22;
      identityFile = "~/.ssh/id_ed25519";
      compression = false;
      forwardAgent = true;
      addKeysToAgent = "yes";
    };
    "dvalar" = {
      hostname = "dvalar.home";
      user = "phdenzel";
      port = 22;
      identityFile = "~/.ssh/id_ed25519";
      compression = false;
      forwardAgent = true;
      addKeysToAgent = "yes";
    };
    "duneyr" = {
      hostname = "dain.home";
      user = "phdenzel";
      port = 22;
      identityFile = "~/.ssh/id_ed25519";
      compression = false;
      forwardAgent = true;
      addKeysToAgent = "yes";
    };
    "phinix" = {
      hostname = "phinix.home";
      user = "phdenzel";
      port = 22;
      identityFile = "~/.ssh/id_ed25519";
      compression = false;
      forwardAgent = true;
      addKeysToAgent = "yes";
    };
    "sol" = {
      hostname = "sol.home";
      user = "phdenzel";
      port = 22;
      identityFile = "~/.ssh/id_ed25519";
      compression = false;
      forwardAgent = true;
      addKeysToAgent = "yes";
    };
    "fenrix" = {
      hostname = "fenrix.home";
      user = "phdenzel";
      port = 22;
      identityFile = "~/.ssh/id_ed25519";
      compression = false;
      forwardAgent = true;
      addKeysToAgent = "yes";
    };
    "asahi" = {
      hostname = "asahi.home";
      user = "phdenzel";
      port = 22;
      identityFile = "~/.ssh/id_ed25519";
      compression = false;
      forwardAgent = true;
      addKeysToAgent = "yes";
    };

    # Research clusters
    "ela" = {
      hostname = "ela.cscs.ch";
      user = "pdenzel";
      identityFile = "~/.ssh/cscs_signed_key";
      compression = false;
      forwardAgent = true;
      addKeysToAgent = "yes";
    };
    "daint" = {
      hostname = "daint.alps.cscs.ch";
      user = "pdenzel";
      identityFile = "~/.ssh/cscs_signed_key";
      proxyJump = "ela";
      addKeysToAgent = "yes";
      identitiesOnly = true;
    };
    "eiger" = {
      hostname = "eiger.alps.cscs.ch";
      user = "pdenzel";
      identityFile = "~/.ssh/cscs_signed_key";
      proxyJump = "ela";      
      addKeysToAgent = "yes";
      identitiesOnly = true;
    };

    "austin" = {
      hostname = "austin.zhaw.ch";
      user = "denp";
      port = 22;
      identityFile = "~/.ssh/dgx_id_ed25519";
      compression = false;
      forwardAgent = true;
      addKeysToAgent = "yes";
    };
    "dallas" = {
      hostname = "dallas.zhaw.ch";
      user = "denp";
      port = 22;
      identityFile = "~/.ssh/dgx_id_ed25519";
      compression = false;
      forwardAgent = true;
      addKeysToAgent = "yes";
    };
    "losangeles" = {
      hostname = "losangeles.zhaw.ch";
      user = "denp";
      port = 22;
      identityFile = "~/.ssh/dgx_id_ed25519";
      compression = false;
      forwardAgent = true;
      addKeysToAgent = "yes";
    };
    "sanfrancisco" = {
      hostname = "sanfrancisco.zhaw.ch";
      user = "denp";
      port = 22;
      identityFile = "~/.ssh/dgx_id_ed25519";
      compression = false;
      forwardAgent = true;
      addKeysToAgent = "yes";
    };
    "sacramento" = {
      hostname = "sacramento.zhaw.ch";
      user = "denp";
      port = 22;
      identityFile = "~/.ssh/dgx_id_ed25519";
      compression = false;
      forwardAgent = true;
      addKeysToAgent = "yes";
    };
    "sanjose" = {
      hostname = "sanjose.zhaw.ch";
      user = "denp";
      port = 22;
      identityFile = "~/.ssh/dgx_id_ed25519";
      compression = false;
      forwardAgent = true;
      addKeysToAgent = "yes";
    };
    "fresko" = {
      hostname = ".zhaw.ch";
      user = "denp";
      port = 22;
      identityFile = "~/.ssh/dgx_id_ed25519";
      compression = false;
      forwardAgent = true;
      addKeysToAgent = "yes";
    };
    "trinity" = {
      hostname = "trinity.zhaw.ch";
      user = "denp";
      port = 22;
      identityFile = "~/.ssh/dgx_id_ed25519";
      compression = false;
      forwardAgent = true;
      addKeysToAgent = "yes";
    };
    "elpaso" = {
      hostname = "elpaso.zhaw.ch";
      user = "denp";
      port = 22;
      identityFile = "~/.ssh/dgx_id_ed25519";
      compression = false;
      forwardAgent = true;
      addKeysToAgent = "yes";
    };
    "fortworth" = {
      hostname = "fortworth.zhaw.ch";
      user = "denp";
      port = 22;
      identityFile = "~/.ssh/dgx_id_ed25519";
      compression = false;
      forwardAgent = true;
      addKeysToAgent = "yes";
    };
    "lubbock" = {
      hostname = "lubbock.zhaw.ch";
      user = "denp";
      port = 22;
      identityFile = "~/.ssh/dgx_id_ed25519";
      compression = false;
      forwardAgent = true;
      addKeysToAgent = "yes";
    };
  };
}
