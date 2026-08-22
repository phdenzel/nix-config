# NVIDIA CUDA toolkit (linux-only)
{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    cudaPackages.cudatoolkit
    cudaPackages.cuda_cudart
    cudaPackages.cuda_nvcc
    cudaPackages.cccl
    cudaPackages.cudnn
  ];
}
