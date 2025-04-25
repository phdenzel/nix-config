{...}: {
  programs.ssh.matchBlocks = {
    # Git platforms
    "gitlab.com" = {
      hostname = "gitlab.com";
      user = "git";
      identityFile = "~/.ssh/gl_id_ed25519";
    };
    "github.com" = {
      hostname = "github.com";
      user = "git";
      identityFile = "~/.ssh/gh_id_ed25519";
    };
    "github.zhaw.ch" = {
      hostname = "github.zhaw.ch";
      user = "git";
      identityFile = "~/.ssh/ghzhaw_id_ed25519";
    };

    # Home environment
    "idun" = {
      hostname = "idun.home";
      user = "phdenzel";
      port = 22220;
      identityFile = "~/.ssh/id_ed25519";
    };
    "ygdrasil" = {
      hostname = "ygdrasil.home";
      user = "phdenzel";
      port = 22;
      identityFile = "~/.ssh/id_ed25519";
    };
    "loki" = {
      hostname = "loki.home";
      user = "phdenzel";
      port = 22;
      identityFile = "~/.ssh/id_ed25519";
    };
    "hugin" = {
      hostname = "hugin.home";
      user = "phdenzel";
      port = 22;
      identityFile = "~/.ssh/id_ed25519";
    };
    "munin" = {
      hostname = "munin.home";
      user = "phdenzel";
      port = 22;
      identityFile = "~/.ssh/id_ed25519";
    };
    "durathror" = {
      hostname = "durathror.home";
      user = "phdenzel";
      port = 22;
      identityFile = "~/.ssh/id_ed25519";
    };
    "dvalar" = {
      hostname = "dvalar.home";
      user = "phdenzel";
      port = 22;
      identityFile = "~/.ssh/id_ed25519";
    };
    "dain" = {
      hostname = "dain.home";
      user = "phdenzel";
      port = 22;
      identityFile = "~/.ssh/id_ed25519";
    };
    "phinix" = {
      hostname = "phinix.home";
      user = "phdenzel";
      port = 22;
      identityFile = "~/.ssh/id_ed25519";
    };
    "fenrix" = {
      hostname = "fenrix.home";
      user = "phdenzel";
      port = 22;
      identityFile = "~/.ssh/id_ed25519";
    };
    "asahi" = {
      hostname = "asahi.home";
      user = "phdenzel";
      port = 22;
      identityFile = "~/.ssh/id_ed25519";
    };

    # Research clusters
    "ela" = {
      hostname = "ela.cscs.ch";
      user = "pdenzel";
      identityFile = "~/.ssh/cscs_signed_key";
    };
    "daint" = {
      hostname = "daint.cscs.ch";
      user = "pdenzel";
      identityFile = "~/.ssh/cscs_signed_key";
      proxyJump = "ela";
    };

    "austin" = {
      hostname = "austin.zhaw.ch";
      user = "denp";
      port = 22;
      identityFile = "~/.ssh/dgx_id_ed25519";
    };
    "dallas" = {
      hostname = "dallas.zhaw.ch";
      user = "denp";
      port = 22;
      identityFile = "~/.ssh/dgx_id_ed25519";
    };
    "losangeles" = {
      hostname = "losangeles.zhaw.ch";
      user = "denp";
      port = 22;
      identityFile = "~/.ssh/dgx_id_ed25519";
    };
    "sanfrancisco" = {
      hostname = "sanfrancisco.zhaw.ch";
      user = "denp";
      port = 22;
      identityFile = "~/.ssh/dgx_id_ed25519";
    };
    "sacramento" = {
      hostname = "sacramento.zhaw.ch";
      user = "denp";
      port = 22;
      identityFile = "~/.ssh/dgx_id_ed25519";
    };
    "sanjose" = {
      hostname = "sanjose.zhaw.ch";
      user = "denp";
      port = 22;
      identityFile = "~/.ssh/dgx_id_ed25519";
    };
    "fresko" = {
      hostname = ".zhaw.ch";
      user = "denp";
      port = 22;
      identityFile = "~/.ssh/dgx_id_ed25519";
    };
    "trinity" = {
      hostname = "trinity.zhaw.ch";
      user = "denp";
      port = 22;
      identityFile = "~/.ssh/dgx_id_ed25519";
    };
    "elpaso" = {
      hostname = "elpaso.zhaw.ch";
      user = "denp";
      port = 22;
      identityFile = "~/.ssh/dgx_id_ed25519";
    };
    "fortworth" = {
      hostname = "fortworth.zhaw.ch";
      user = "denp";
      port = 22;
      identityFile = "~/.ssh/dgx_id_ed25519";
    };
    "lubbock" = {
      hostname = "lubbock.zhaw.ch";
      user = "denp";
      port = 22;
      identityFile = "~/.ssh/dgx_id_ed25519";
    };
  };
}
