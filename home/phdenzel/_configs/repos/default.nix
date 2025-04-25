{
  inputs,
  config,
  ...
}: {
  # emacs packages as (pinned) read-only git clones in nix-store registry
  programs.emacs.extraPackages = epkgs: [
    (epkgs.trivialBuild {
      pname = "phd-ark-modeline";
      src = inputs.phd-ark-modeline;
      packageRequires = with epkgs; [all-the-icons-nerd-fonts flycheck];
      version = "0.1.0";
      preferLocalBuild = true;
      allowSubstitutes = false;
    })
    (epkgs.trivialBuild {
      pname = "phd-ark-tabline";
      src = inputs.phd-ark-tabline;
      packageRequires = [epkgs.s];
      version = "0.1.0";
      preferLocalBuild = true;
      allowSubstitutes = false;
    })
  ];
  home.file.phd-ark-modeline = {
    source = "${inputs.phd-ark-modeline}/phd-ark-modeline.el";
    target = ".emacs.d/phd-ark-modeline.el";
  };
  home.file.phd-ark-tabline = {
    source = "${inputs.phd-ark-tabline}/phd-ark-tabline.el";
    target = ".emacs.d/phd-ark-tabline.el";
  };

  # wallpapers (pinned) read-only git clone in nix-store registry
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
      # home repos
      nix-config = {
        checkout = "git clone git@github.com:phdenzel/nix-config.git";
      };
      ".password-store" = {
        checkout = "git clone git@github.com:phdenzel/.password-store.git";
      };
      zettelkasten = {
        checkout = "git clone git@github.com:phdenzel/zettelkasten.git";
      };
      slides = {
        checkout = "git clone git@github.com:phdenzel/slides.git";
      };
      "phdenzel.github.io" = {
        checkout = "git clone git@github.com:phdenzel/phdenzel.github.io.git";
      };
      chuchichaestli = {
        checkout = "git clone git@github.com:CAIIVS/chuchichaestli.git";
      };
      skais = {
        checkout = "git clone git@github.com:phdenzel/skais.git";
      };
      zmk-config = {
        checkout = "git clone git@github.com:phdenzel/zmk-config.git";
      };

      # documents repos
      "Documents/PhDCV" = {
        checkout = "git clone git@github.com:phdenzel/PhDCV.git";
      };
      # "Documents/letters" = {
      #   checkout = "git clone git@github.com:phdenzel/letters.git";
      # };
      "Documents/phd-thesis" = {
        checkout = "git clone git@github.com:phdenzel/phd-thesis.git";
      };
      "Documents/master-thesis" = {
        checkout = "git clone git@github.com:phdenzel/master-thesis.git";
      };

      # papers
      "Documents/papers/galactic-alchemy-1" = {
        checkout = "git clone git@github.com:phdenzel/galactic-alchemy-1.git";
      };
      "Documents/papers/sds25-ml4gleam" = {
        checkout = "git clone git@github.com:phdenzel/sds25-ml4gleam.git";
      };
      "Documents/papers/saia24-pract-cert" = {
        checkout = "git clone git@github.com:phdenzel/saia24-pract-cert.git";
      };
      "Documents/papers/sds24-cert-ais" = {
        checkout = "git clone git@github.com:phdenzel/sds24-cert-ais.git";
      };
      "Documents/papers/sds24-mlops4tai" = {
        checkout = "git clone git@github.com:phdenzel/sds24-mlops4tai.git";
      };
      "Documents/papers/slacs-matching" = {
        checkout = "git clone git@github.com:phdenzel/slacs-matching-paper.git slacs-matching";
      };
      "Documents/papers/time-delays" = {
        checkout = "git clone git@github.com:phdenzel/time-delays-paper.git time-delays";
      };
      "Documents/papers/sw05" = {
        checkout = "git clone git@github.com:phdenzel/sw05-paper.git sw05";
      };
      "Documents/papers/adler" = {
        checkout = "git clone git@github.com:phdenzel/adler-paper.git adler";
      };
      "Documents/papers/heat-spike" = {
        checkout = "git clone git@github.com:phdenzel/heat-spike-paper.git heat-spike";
      };

      # proposals
      "Documents/proposals/snf_spark25_radigral" = {
        checkout = "git clone git@github.com:phdenzel/snf_spark25_radigral.git";
      };
      "Documents/proposals/snf_spark23_gl3dgen" = {
        checkout = "git clone git@github.com:phdenzel/snf_spark23_gl3dgen.git";
      };
      "Documents/proposals/snf_project_dg3nerate" = {
        checkout = "git clone git@github.com:phdenzel/snf_project_dg3nerate.git";
      };

      # posters
      "Documents/posters/poster-mlops4tai" = {
        checkout = "git clone git@github.com:phdenzel/poster-mlops4tai.git";
      };

      # local repos
      "local/wallpapers" = {
        checkout = "git clone git@github.com:phdenzel/wallpapers.git";
      };
      "local/nix-systems" = {
        checkout = "git clone git@github.com:phdenzel/nix-systems.git";
      };
      "local/phd-ark-modeline" = {
        checkout = "git clone git@github.com:phdenzel/phd-ark-modeline.git";
      };
      "local/phd-ark-tabline" = {
        checkout = "git clone git@github.com:phdenzel/phd-ark-tabline.git";
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
      "local/pyphd" = {
        checkout = "git clone git@github.com:phdenzel/pyphd.git";
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

      # archived repos
      "local/archived/pentaplex" = {
        checkout = "git clone git@github.com:phdenzel/pentaplex.git";
      };
      "local/archived/twilio-sms-bot" = {
        checkout = "git clone git@github.com:phdenzel/twilio-sms-bot.git";
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
    };
  };
}
