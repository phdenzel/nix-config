{config, inputs, ...}: let
  homeDir = config.home.homeDirectory;
  userName = config.home.username;
in {
  imports = [
    inputs.sops-nix.homeManagerModules.sops
  ];
  sops.templates.".pypirc" = {
    content = ''
      [testpypi]
        username = __token__
        password = ${config.sops.placeholder."pypi/${userName}/testpypi"}
      
      [pypi]
        username = __token__
        password = ${config.sops.placeholder."pypi/${userName}/pypi"}
    '';
    path = "${homeDir}/.pypirc";
  };
}
