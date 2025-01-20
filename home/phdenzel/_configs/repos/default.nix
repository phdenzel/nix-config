{inputs, config, ...}: {
  # another (pinned) read-only wallpaper clone in nix-store registry
  home.file.wallpapers = {
    source = inputs.phd-wallpapers;
    target = config.xdg.userDirs.pictures + "/wallpapers";
  };
  stylix = {
    image = config.xdg.userDirs.pictures + "/wallpapers/ethereal_4k.png";
    imageScalingMode = "fill";
  };

  programs.mr = {
    enable = true;
    settings = {
      "local/wallpapers" = {
        checkout = "git clone git@github.com:phdenzel/wallpapers.git";
      };
      "local/phd-dashboard" = {
        checkout = "git clone git@github.com:phdenzel/phd-dashboard.git";
      };
      "local/phd-modeline" = {
        checkout = "git clone git@github.com:phdenzel/phd-modeline.git";
      };
      "local/phd-mu4e-setup" = {
        checkout = "git clone git@github.com:phdenzel/phd-mu4e-setup.git";
      };
      "local/btrsnap" = {
        checkout = "git clone git@github.com:phdenzel/btrsnap.git";
      };
      "local/cscs-sshservice-cli" = {
        checkout = "git clone git@github.com:phdenzel/cscs-sshservice-cli.git";
      };
      "local/hatch-bump" = {
        checkout = "git clone git@github.com:phdenzel/hatch-bump.git";
      };
      "local/camengine.js" = {
        checkout = "git clone git@github.com:phdenzel/camengine.js.git";
      };
      "local/lensing.js" = {
        checkout = "git clone git@github.com:phdenzel/lensing.js.git";
      };
      "local/streaming-lens" = {
        checkout = "git clone git@github.com:phdenzel/streaming-lens.git";
      };
      "local/zurich-lens" = {
        checkout = "git clone git@github.com:phdenzel/zurich-lens.git";
      };
      "local/reveal.js" = {
        checkout = "git clone git@github.com:phdenzel/reveal.js.git";
      };
      "local/archived/julia" = {
        checkout = "git clone git@github.com:phdenzel/julia.git";
      };
      "local/archived/dydama" = {
        checkout = "git clone git@github.com:phdenzel/dydama.git";
      };
      "local/archived/kdtree-p" = {
        checkout = "git clone git@github.com:phdenzel/kdtree-p.git";
      };
      "local/archived/ising" = {
        checkout = "git clone git@github.com:phdenzel/ising.git";
      };
      "local/archived/fibonacci-flower" = {
        checkout = "git clone git@github.com:phdenzel/fibonacci-flower.git";
      };

      "phdenzel.github.io" = {
        checkout = "git clone git@github.com:phdenzel/phdenzel.github.io.git";
      };
      slides = {
        checkout = "git clone git@github.com:phdenzel/slides.git";
      };
      zmk-config = {
        checkout = "git clone git@github.com:phdenzel/zmk-config.git";
      };
      ".password-store" = {
        checkout = "git clone git@github.com:phdenzel/.password-store.git";
      };
      nix-config = {
        checkout = "git clone git@github.com:phdenzel/nix-config.git";
      };
    };
  };
}
