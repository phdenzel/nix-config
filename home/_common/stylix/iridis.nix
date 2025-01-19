{config, ...}: let
  theme = {
    
  };
in {
  imports = [
    ../../../modules/colors.nix
  ];
  colorScheme = {
    slug = "phd-ark-iridis";
    palette = rec {
      magenta = "FF0087";
      pink = "E83A82";
      ruby = "D7005F";
      crimson = "D70000";
      red = "FF6D6B";
      tiger = "FF5F00";
      orange = "F69927";
      sand = "FDB760";
      yellow = "FFD787";
      green = "44BC84";
      grass = "3DAA77";
      emerald = "00AF5F";
      viridis = "00AF87";
      teal = "4DB5BD";
      ocean = "1F5582";
      turquoise = "1AE6CC";
      cyan = "46D9FF";
      blue = "5F8AF7";
      indigo = "5F5FFF";
      amethyst = "3723B7";
      lilac = "875FFF";
      purple = "8787FF";
      violet = "A9A1E1";
      white = "F6F9FE";
      subtext0 = "EDF3FE";
      subtext1 = "DDE3EE";
      text = "BDC3CE";
      overlay2 = "8A8A8A";
      overlay1 = "464858";
      overlay0 = "5B6268";
      surface2 = "515C66";
      surface1 = "404850";
      surface0 = "3A3C3F";
      base = "303033";
      mantle = "2B2B2F";
      crust = "181E26";
      overlay256 = "000";
      red256 = "001";
      green256 = "002";
      yellow256 = "003";
      blue256 = "004";
      pink256 = "005";
      teal256 = "006";
      subtext256 = "007";
      purple256 = "105";
      surface256 = "008";
      ruby256 = "009";
      viridis256 = "010";
      sand256 = "011";
      indigo256 = "012";
      magenta256 = "013";
      cyan256 = "014";
      white256 = "015";
      lilac256 = "099";
    };
  };
  stylix = {
    base16Scheme = {
      name = "phd-ark-iridis";
      author = "phdenzel";
      variant = "dark";
      palette = {
        base00 = config.colorScheme.palette.base; # default background
        base01 = config.colorScheme.palette.surface0; # alternate background
        base02 = config.colorScheme.palette.overlay1; # selection background
        base03 = config.colorScheme.palette.overlay2; # comments
        base04 = config.colorScheme.palette.teal; # alternate foreground
        base05 = config.colorScheme.palette.text; # default foreground
        base06 = config.colorScheme.palette.subtext0; # lighter foreground
        base07 = config.colorScheme.palette.surface2; # lighter background
        base08 = config.colorScheme.palette.ruby; # red
        base09 = config.colorScheme.palette.sand; # orange
        base0A = config.colorScheme.palette.yellow; # yellow
        base0B = config.colorScheme.palette.green; # green
        base0C = config.colorScheme.palette.cyan; # cyan
        base0D = config.colorScheme.palette.blue; # blue
        base0E = config.colorScheme.palette.purple; # purple
        base0F = config.colorScheme.palette.crimson; # dark red
      };
    };
    override = {
      base10 = config.colorScheme.palette.mantle; # darker background
      base11 = config.colorScheme.palette.crust; # darkest background
      base12 = config.colorScheme.palette.pink; # bright red
      base13 = config.colorScheme.palette.orange; # bright orange
      base14 = config.colorScheme.palette.viridis; # bright green
      base15 = config.colorScheme.palette.teal; # bright cyan
      base16 = config.colorScheme.palette.indigo; # bright blue
      base17 = config.colorScheme.palette.violet; # bright purple
    };
  };
}
