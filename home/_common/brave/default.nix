{pkgs, ...}: {
  programs.chromium = {
    enable = true;
    package = pkgs.brave;
    extensions = [
      { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; } # uBlock Origin
      { id = "eimadpbcbfnmbkopoojfekhnkhdbieeh"; } # Dark Reader
      { id = "lcbjdhceifofjlpecfpeimnnphbcjgnc"; } # xBrowserSync
    ];
  };

  programs.browserpass = {
    enable = true;
    browsers = ["brave"];
  };
}
