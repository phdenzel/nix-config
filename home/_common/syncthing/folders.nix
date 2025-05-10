{lib, ...}:
with lib; let
  allDevices = ["phinix" "fenrix" "asahi"];
  homeDevices = ["phinix" "asahi"];
  allSyncs = [
    "Sync"
    "cai-seminar-doc"
    "chuchichaestli"
    "cluster"
    "Documents"
    "Experiments"
    "mdq-doc"
    "org"
    "Playground"
    "Projects"
    "skach"
    "slides"
    "teaching"
    "zettelkasten"
  ];
  homeSyncs = [
    "Music"
    "Pictures"
  ];
  ignoreFile = ''
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
in {
  services.syncthing.settings.folders =
    attrsets.mergeAttrsList (
      lists.map (name: {
        "${name}" = {
          path = "~/${name}";
          devices = allDevices;
          ignorePerms = false;
        };
      })
      allSyncs
    )
    // attrsets.mergeAttrsList (
      lists.map (name: {
        "${name}" = {
          path = "~/${name}";
          devices = homeDevices;
          ignorePerms = false;
        };
      })
      homeSyncs
    );

  home.file = attrsets.mergeAttrsList (
    lists.map (name: {
      "${name}/.stignore".text = ignoreFile;
    })
    (allSyncs ++ homeSyncs)
  );
}
