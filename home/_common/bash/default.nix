{
  pkgs,
  ...
}: {

  programs.bash = {
    enable = true;
    enableCompletion = true;

    initExtra = ''
      fastfetch
    '';

    shellOptions = [
      "histappend"
      "checkwinsize"
      "extglob"
      "globstar"
      "checkjobs"
      "autocd"
      "cdspell"
    ];

    historyControl = ["ignoreboth"];
    historySize = 65536;
    historyFileSize = 262144;
  };

  home.packages = with pkgs; [
    bash-completion
  ];
}
