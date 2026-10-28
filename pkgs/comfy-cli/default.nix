{
  lib,
  python3Packages,
  fetchPypi,
}:
python3Packages.buildPythonApplication rec {
  pname = "comfy-cli";
  version = "1.23.0";
  pyproject = true;

  src = fetchPypi {
    pname = "comfy_cli";
    inherit version;
    hash = "sha256-e/07q9XJPlnndioXpSJldhDI6JBV7vjgnQTGiQ2vKrY=";
  };

  build-system = with python3Packages; [setuptools];

  dependencies = with python3Packages; [
    blake3
    certifi
    charset-normalizer
    cookiecutter
    gitpython
    httpx
    imageio-ffmpeg
    packaging
    pathspec
    psutil
    pyyaml
    questionary
    requests
    rich
    ruff
    semver
    tomlkit
    typer
    typing-extensions
    uv
    websocket-client
  ];

  # nixpkgs ships newer blake3 and semver than upstream pins
  pythonRelaxDeps = ["blake3" "semver"];

  # Telemetry clients are only imported lazily; nixpkgs marks posthog broken
  pythonRemoveDeps = ["mixpanel" "posthog"];
  makeWrapperArgs = ["--set-default" "DO_NOT_TRACK" "1"];

  pythonImportsCheck = ["comfy_cli"];

  meta = {
    description = "CLI tool for installing and using ComfyUI";
    homepage = "https://github.com/Comfy-Org/comfy-cli";
    license = lib.licenses.gpl3Only;
    mainProgram = "comfy";
  };
}
