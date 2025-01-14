{...}: let
  theme = {
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
  };
in {
  stylix = {
    base16Scheme = {
      system = "base16";
      name = "phd-ark-iridis";
      author = "phdenzel";
      variant = "dark";
      palette = {
        base00 = theme.base; # default background
        base01 = theme.surface0; # alternate background
        base02 = theme.overlay1; # selection background
        base03 = theme.overlay2; # comments
        base04 = theme.teal; # alternate foreground
        base05 = theme.text; # default foreground
        base06 = theme.subtext0; # lighter foreground
        base07 = theme.surface2; # lighter background
        base08 = theme.ruby; # red
        base09 = theme.sand; # orange
        base0A = theme.yellow; # yellow
        base0B = theme.green; # green
        base0C = theme.cyan; # cyan
        base0D = theme.blue; # blue
        base0E = theme.purple; # purple
        base0F = theme.crimson; # dark red
        # base10 = theme.mantle; # darker background
        # base11 = theme.crust; # darkest background
        # base12 = theme.pink; # bright red
        # base13 = theme.orange; # bright orange
        # base14 = theme.viridis; # bright green
        # base15 = theme.teal; # bright cyan
        # base16 = theme.indigo; # bright blue
        # base17 = theme.violet; # bright purple
      };
    };
  };
}
