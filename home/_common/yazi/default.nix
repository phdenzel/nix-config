{pkgs, ...}: {
  programs.yazi = {
    enable = true;
    enableZshIntegration = true;
    shellWrapperName = "y";  # type `yy` in shell to exit in cwd

    plugins = with pkgs.yaziPlugins; {
      chmod = chmod;
      compress = compress;
      git = git;
      starship = starship;
    };

    initLua = ''
			require("starship"):setup()
      require("git"):setup()
		'';

    keymap = {
      mgr.prepend_keymap = [
        {
          on = ["c" "m"];
          run = "plugin chmod";
          desc = "Chmod on selected files";
        }
        {
          on = ["c" "a" "a"];
          run = "plugin compress";
          desc = "Archive selected files";
        }
        {
          on = ["c" "a" "l"];
          run = "plugin compress -l";
          desc = "Archive selected files (compression level)";
        }
        {
          on = ["c" "a" "t"];
          run = "plugin compress -l tar.gz";
          desc = "Archive selected files (tarball)";
        }
      ];
    };
  };
}
