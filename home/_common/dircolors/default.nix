{
  lib,
  config,
  ...
}:
with lib; let
  mkColor = {
    color,
    type ? "",
  }: "${type}38;5;${color}";
  mkFBColor = {
    fgColor,
    bgColor,
    type ? "",
  }: "${type}38;5;${fgColor};48;5;${bgColor}";
in {
  programs.dircolors = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
    enableFishIntegration = true;
    extraConfig  = ''
      COLORTERM ?*
      TERM Eterm
      TERM ansi
      TERM *color*
      TERM con[0-9]*x[0-9]*
      TERM cons25
      TERM console
      TERM cygwin
      TERM *direct*
      TERM dtterm
      TERM gnome
      TERM hurd
      TERM jfbterm
      TERM konsole
      TERM kterm
      TERM linux
      TERM linux-c
      TERM mlterm
      TERM putty
      TERM rxvt*
      TERM screen*
      TERM st
      TERM terminator
      TERM tmux*
      TERM vt100
      TERM xterm*
    '';
    settings = with config.colorScheme.palette256; {
      RESET = "0";
      MULTIHARDLINK = "00";
      DIR = mkColor {
        color = indigo256;
        type = "01;";
      };
      LINK = mkColor {
        color = viridis256;
        type = "04;";
      };
      EXEC = mkColor {
        color = magenta256;
        type = "01;";
      };
      BLK = mkColor {
        color = cyan256;
        type = "01;";
      };
      CHR = mkColor {
        color = teal256;
        type = "01;";
      };
      SOCK = mkColor {
        color = sand256;
        type = "01;04;";
      };
      DOOR = mkColor {
        color = yellow256;
        type = "01;02;04;";
      };
      FIFO = mkColor {
        color = viridis256;
        type = "01;04;";
      };
      ORPHAN = mkColor {
        color = subtext256;
        type = "02;";
      };
      MISSING = mkColor {
        color = overlay256;
        type = "02;";
      };
      SETUID = mkFBColor {
        fgColor = blue256;
        bgColor = white256;
        type = "07;";
      };
      SETGID = mkFBColor {
        fgColor = teal256;
        bgColor = white256;
        type = "07;";
      };
      CAPABILITY = mkFBColor {
        fgColor = white256;
        bgColor = overlay256;
        type = "07;";
      };
      STICKY_OTHER_WRITABLE = mkColor {
        color = ruby256;
        type = "01;";
      };
      OTHER_WRITABLE = mkColor {
        color = red256;
        type = "01;";
      };
      STICKY = mkColor {
        color = sand256;
        type = "01;";
      };
      # File extensions
      # DOS-specific
      ".exe" = mkColor {color = teal256;};
      ".cmd" = mkColor {color = teal256;};
      # Backup files
      "*~" = mkColor {
        color = subtext256;
        type = "02;";
      };
      ".bak" = mkColor {
        color = subtext256;
        type = "02;";
      };
      ".BAK" = mkColor {
        color = subtext256;
        type = "02;";
      };
      ".old" = mkColor {
        color = subtext256;
        type = "02;";
      };
      ".OLD" = mkColor {
        color = subtext256;
        type = "02;";
      };
      ".orig" = mkColor {
        color = subtext256;
        type = "02;";
      };
      ".ORIG" = mkColor {
        color = subtext256;
        type = "02;";
      };
      ".swp" = mkColor {
        color = subtext256;
        type = "02;";
      };
      ".log" = mkColor {
        color = subtext256;
        type = "02;";
      };
      # Compressed files
      ".tar" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".tgz" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".arc" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".arj" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".taz" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".lha" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".lz4" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".lzh" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".lzma" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".tlz" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".txz" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".tzo" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".t7z" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".zip" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".z" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".Z" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".dz" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".gz" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".lrz" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".lz" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".lzo" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".xz" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".zst" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".tzst" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".bz2" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".bz" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".tbz" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".tbz2" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".tz" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".deb" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".rpm" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".jar" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".war" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".ear" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".sar" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".rar" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".alz" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".ace" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".zoo" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".cpio" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".7z" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".rz" = mkColor {
        color = lilac256;
        type = "01;";
      };
      ".cab" = mkColor {
        color = lilac256;
        type = "01;";
      };
      # Image formats
      ".jpg" = mkColor {color = sand256;};
      ".JPG" = mkColor {color = sand256;};
      ".jpeg" = mkColor {color = sand256;};
      ".JPEG" = mkColor {color = sand256;};
      ".bmp" = mkColor {color = sand256;};
      ".pbm" = mkColor {color = sand256;};
      ".pgm" = mkColor {color = sand256;};
      ".ppm" = mkColor {color = sand256;};
      ".pps" = mkColor {color = sand256;};
      ".ppsx" = mkColor {color = sand256;};
      ".tga" = mkColor {color = sand256;};
      ".xbm" = mkColor {color = sand256;};
      ".xpm" = mkColor {color = sand256;};
      ".tif" = mkColor {color = sand256;};
      ".tiff" = mkColor {color = sand256;};
      ".png" = mkColor {color = sand256;};
      ".PNG" = mkColor {color = sand256;};
      ".pcx" = mkColor {color = sand256;};
      ".flc" = mkColor {color = sand256;};
      ".fli" = mkColor {color = sand256;};
      ".gl" = mkColor {color = sand256;};
      ".dl" = mkColor {color = sand256;};
      ".xcf" = mkColor {color = sand256;};
      ".xwd" = mkColor {color = sand256;};
      ".emf" = mkColor {color = sand256;};
      ".ico" = mkColor {color = sand256;};
      ".gif" = mkColor {color = sand256;};
      ".gifv" = mkColor {color = sand256;};
      ".mjpg" = mkColor {color = sand256;};
      ".mjpeg" = mkColor {color = sand256;};
      # Image formats for astrophysicists and astronomers ;)
      ".fits" = mkColor {color = yellow256;};
      ".fts" = mkColor {color = yellow256;};
      # Vectorized graphics formats
      ".pdf" = mkColor {color = ruby256;};
      ".dvi" = mkColor {color = ruby256;};
      ".cgm" = mkColor {color = ruby256;};
      ".dvg" = mkColor {color = ruby256;};
      ".eps" = mkColor {color = ruby256;};
      ".ps" = mkColor {color = ruby256;};
      ".odg" = mkColor {color = ruby256;};
      ".mba" = mkColor {color = ruby256;};
      ".svg" = mkColor {color = ruby256;};
      ".svgz" = mkColor {color = ruby256;};
      ".swf" = mkColor {color = ruby256;};
      ".pgf" = mkColor {color = ruby256;};
      ".tikz" = mkColor {color = ruby256;};
      ".rvt" = mkColor {color = ruby256;};
      ".sxd" = mkColor {color = ruby256;};
      # Video formats
      ".webm" = mkColor {color = blue256;};
      ".mkv" = mkColor {color = blue256;};
      ".flv" = mkColor {color = blue256;};
      ".vob" = mkColor {color = blue256;};
      ".ogv" = mkColor {color = blue256;};
      ".ogm" = mkColor {color = blue256;};
      ".ogx" = mkColor {color = blue256;};
      ".drc" = mkColor {color = blue256;};
      ".mng" = mkColor {color = blue256;};
      ".avi" = mkColor {color = blue256;};
      ".mov" = mkColor {color = blue256;};
      ".MOV" = mkColor {color = blue256;};
      ".qt" = mkColor {color = blue256;};
      ".wmv" = mkColor {color = blue256;};
      ".yuv" = mkColor {color = blue256;};
      ".rm" = mkColor {color = blue256;};
      ".rmvb" = mkColor {color = blue256;};
      ".asf" = mkColor {color = blue256;};
      ".amv" = mkColor {color = blue256;};
      ".mp4" = mkColor {color = blue256;};
      ".m4p" = mkColor {color = blue256;};
      ".m4v" = mkColor {color = blue256;};
      ".mp4v" = mkColor {color = blue256;};
      ".mpg" = mkColor {color = blue256;};
      ".mpeg" = mkColor {color = blue256;};
      ".mpe" = mkColor {color = blue256;};
      ".mpv" = mkColor {color = blue256;};
      ".mp2" = mkColor {color = blue256;};
      ".m2v" = mkColor {color = blue256;};
      ".svi" = mkColor {color = blue256;};
      ".3gp" = mkColor {color = blue256;};
      ".3g2" = mkColor {color = blue256;};
      ".mxf" = mkColor {color = blue256;};
      ".roq" = mkColor {color = blue256;};
      ".nsv" = mkColor {color = blue256;};
      ".nuv" = mkColor {color = blue256;};
      ".f4v" = mkColor {color = blue256;};
      ".f4p" = mkColor {color = blue256;};
      ".f4a" = mkColor {color = blue256;};
      ".f4b" = mkColor {color = blue256;};
      # Audio formats
      ".aac" = mkColor {color = cyan256;};
      ".au" = mkColor {color = cyan256;};
      ".flac" = mkColor {color = cyan256;};
      ".m4a" = mkColor {color = cyan256;};
      ".mid" = mkColor {color = cyan256;};
      ".midi" = mkColor {color = cyan256;};
      ".mka" = mkColor {color = cyan256;};
      ".mp3" = mkColor {color = cyan256;};
      ".mpc" = mkColor {color = cyan256;};
      ".ogg" = mkColor {color = cyan256;};
      ".ra" = mkColor {color = cyan256;};
      ".wav" = mkColor {color = cyan256;};
      ".oga" = mkColor {color = cyan256;};
      ".opus" = mkColor {color = cyan256;};
      ".spx" = mkColor {color = cyan256;};
      ".xspf" = mkColor {color = cyan256;};
      # Programming and scripting
      ".sh" = mkColor {color = pink256;};
      ".csh" = mkColor {color = pink256;};
      ".zsh" = mkColor {color = pink256;};
      ".cpp" = mkColor {color = yellow256;};
      ".o" = mkColor {
        color = yellow256;
        type = "02;";
      };
      ".c" = mkColor {color = yellow256;};
      ".py" = mkColor {color = green256;};
      ".pyc" = mkColor {
        color = green256;
        type = "02;";
      };
      ".org" = mkColor {color = purple256;};
      ".rs" = mkColor {color = ruby256;};
      ".nix" = mkColor {color = cyan256;};
    };
  };
}
