{pkgs, ...}: {
  programs.ruff = {
    enable = true;
    package = pkgs.python312Packages.ruff;
    settings = {
      line-length = 120;
      docstring-code-line-length = 180;
      lint = {
        select = ["E" "F" "D"];
        ignore = ["D200" "D212" "D415"];
        pydocstyle = "google";
      };
    };
  };
}
