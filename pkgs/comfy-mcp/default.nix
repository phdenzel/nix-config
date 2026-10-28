{
  lib,
  python3Packages,
  fetchPypi,
  comfy-cli,
}:
python3Packages.buildPythonApplication rec {
  pname = "comfy-mcp";
  version = "0.10.0";
  pyproject = true;

  src = fetchPypi {
    pname = "comfy_mcp";
    inherit version;
    hash = "sha256-GRK3sVfJMrjNrsYmyeRYKuzlTZFU9NdCNiFZBubYUEM=";
  };

  build-system = with python3Packages; [setuptools];

  dependencies = with python3Packages; [
    anyio
    mcp
    pydantic
  ];

  makeWrapperArgs = ["--set-default" "COMFY_BIN" (lib.getExe comfy-cli)];

  pythonImportsCheck = ["comfy_mcp"];

  meta = {
    description = "MCP server for ComfyUI, a thin wrapper over comfy-cli";
    homepage = "https://pypi.org/project/comfy-mcp";
    license = lib.licenses.agpl3Plus;
    mainProgram = "comfy-mcp";
  };
}
