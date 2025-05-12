{lib, config, ...}:
with lib; let
  globalIgnoreFile = ''
    (?d).DS_Store
    (?d).fseventsd
    (?d).Spotlight-V100
    (?d).TemporaryItems
    (?d).Trashes
    (?d).AppleDB
    (?d).AppleDesktop
    (?d).com.apple.timemachine.donotpresent
    (?i)photo booth library
    (?i)photos library.*
    (?i)music library.musiclibrary
    (?i)automatically add to music*
    (?i)previous libraries*
    .Media Preferences.plist
    *.localized
    .venv
    .mypy_cache
    .pytest_cache
    .python-version
  '';
  syncs = import ./_folders.nix;
in {
  home.file =
    attrsets.mergeAttrsList (
      lists.map (name: {
        "${name}/.stignore".text = globalIgnoreFile;
      })
      (syncs.all ++ syncs.home ++ syncs.work)
    );
}
