{
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.colorScheme;
  hexColorType = mkOptionType {
    name = "hex-color";
    descriptionClass = "noun";
    description = "RGB color in hex format";
    check = x: isString x && !(hasPrefix "#" x);
  };
in {
  options.colorScheme = {
    slug = mkOption {
      description = "Color scheme name.";
      type = types.str;
      default = "";
      example = "my-custom-scheme";
    };
    name = mkOption {
      description = "Color scheme title";
      type = types.str;
      default = "";
      example = "My Custom Scheme";
    };
    description = mkOption {
      type = types.str;
      default = "";
      example = "My favorite colors.";
      description = "A short description about the scheme.";
    };
    author = mkOption {
      type = types.str;
      default = "";
      example = "phdenzel";
      description = "Color scheme author";
    };
    polarity = mkOption {
      type = types.enum ["dark" "light"];
      description = "Scheme polarity, either 'dark' or 'light'";
      example = "dark";
      default =
        if builtins.substring 0 1 cfg.palette.base00 < "5"
        then "dark"
        else "light";
    };
    palette = mkOption {
      type = with types;
        attrsOf (
          coercedTo str (removePrefix "#") hexColorType
        );
      default = {};
      example = "";
      description = ''
        An unopinionated color palette (add whatever attributes you want).
        Use baseXX attributes for compatibility with base16/24 colors.
      '';
    };
    palette256 = mkOption {
      type = with types;
        attrsOf (
          coercedTo str (removePrefix "#") hexColorType
        );
      default = {};
      example = "";
      description = ''
        An unopinionated color palette of ANSI colors
        (add whatever attributes you want).
      '';
    };
  };
}
