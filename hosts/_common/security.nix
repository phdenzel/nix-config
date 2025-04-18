{...}: {
  # Security
  security = {
    polkit.enable = true;
    acme.acceptTerms = true;
  };

  # System hardening (iterative benchmarking with lynis)
  #   $ nix-shell -p lynis
  #   $ sudo lynis audit system
  boot.kernelModules = ["tcp_bbr"];
  boot.blacklistedKernelModules = [
    "dccp"
    "sctp"
    "rds"
    "tipc"
  ];
  boot.kernel.sysctl = {
    # disable SysRq key for security concerns
    "kernel.sysrq" = 0;
    # replace kernel pointers with 0's always
    "kernel.kptr_restrict" = 2;
    # diable socket filtering
    "kernel.unprivileged_bpf_disabled" = 1;
    "net.core.bpf_jit_harden" = 2;
    # disable bogus ICMP error spamming
    "net.ipv4.icmp_ignore_bogus_error_responses" = 1;
    # reverse path filtering for source validation
    "net.ipv4.conf.default.rp_filter" = 1;
    "net.ipv4.conf.all.rp_filter" = 1;
    # do not accept IP source route packets
    "net.ipv4.conf.all.accept_source_route" = 0;
    "net.ipv6.conf.all.accept_source_route" = 0;
    # do not send ICMP redirects
    "net.ipv4.conf.all.send_redirects" = 0;
    "net.ipv4.conf.default.send_redirects" = 0;
    # Refuse ICMP redirects (MITM mitigations)
    "net.ipv4.conf.all.accept_redirects" = 0;
    "net.ipv4.conf.default.accept_redirects" = 0;
    "net.ipv6.conf.all.accept_redirects" = 0;
    "net.ipv6.conf.default.accept_redirects" = 0;
    "net.ipv4.conf.all.secure_redirects" = 0;
    "net.ipv4.conf.default.secure_redirects" = 0;
    # log martians
    "net.ipv4.conf.all.log_martians" = 1;
    "net.ipv4.conf.default.log_martians" = 1;
    # protect against SYN flood attacks
    "net.ipv4.tcp_syncookies" = 1;
    # protect against TIME-WAIT assassination
    "net.ipv4.tcp_rfc1337" = 1;

    # TCP optimizations
    "net.ipv4.tcp_fastopen" = 3;  # TCP fast open for incoming and outgoing connections
    # Bufferbloat mitigations
    "net.ipv4.tcp_congestion_control" = "bbr";
    "net.core.default_qdisc" = "cake";
  };
}
