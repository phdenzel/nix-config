{
  lib,
  stdenv,
  kernel,
  kernelModuleMakeFlags,
  fetchFromGitHub,
}:
stdenv.mkDerivation {
  pname = "ec-su-axb35";
  version = "0-unstable-2026-08-20";

  src = fetchFromGitHub {
    owner = "cmetz";
    repo = "ec-su_axb35-linux";
    rev = "f62c2c228959a08683273a26ef3afd8991e69f6d";
    hash = "sha256-R172f44zac7quY8apVi4qnJIPOwZgVPN2DMOJTkg5yg=";
  };

  nativeBuildInputs = kernel.moduleBuildDependencies;

  makeFlags =
    kernelModuleMakeFlags
    ++ [
      "KERNEL_BUILD=${kernel.dev}/lib/modules/${kernel.modDirVersion}/build"
      "INSTALL_MOD_PATH=${placeholder "out"}"
      "INSTALL_MOD_DIR=kernel/drivers/platform/x86"
      "INSTALL_MOD_STRIP=1"
      "DEPMOD=true"
    ];

  buildFlags = ["modules"];

  installTargets = ["modules_install"];

  installFlags = [
    "-C"
    "${kernel.dev}/lib/modules/${kernel.modDirVersion}/build"
    "M=$(PWD)"
  ];

  postInstall = ''
    install -Dm755 scripts/su_axb35_monitor -t $out/bin
  '';

  meta = {
    description = "Embedded controller driver (fans, temperature, APU power mode) for Sixunited AXB35-02 boards";
    homepage = "https://github.com/cmetz/ec-su_axb35-linux";
    license = lib.licenses.gpl2Only;
    platforms = ["x86_64-linux"];
  };
}
