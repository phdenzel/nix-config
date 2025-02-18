{pkgs, config, ...}: {
  imports = [./nerd-fonts.nix ./disabled.nix];
  
  programs.starship = with config.colorScheme.palette; {
    enable = true;
    package = pkgs.starship;
    settings = {
      add_newline = true;
      format = "$username$hostname$localip$shlvl$singularity$kubernetes$directory$git_branch$git_commit$git_state$git_metrics$git_status$docker_context$package$c$cmake$dotnet$elixir$elm$erlang$golang$haskell$helm$java$julia$gradle$lua$nodejs$perl$php$python$ruby$rust$scala$solidity$terraform$vagrant$zig$buf$nix_shell$conda$meson$spack$memory_usage$aws$gcloud$openstack$azure$direnv$env_var$custom$line_break$jobs$time$status$os$container$shell$character";
      right_format = "$cmd_duration$sudo";
      character = {
        # success_symbol = "[](blue)[](bold green)";
        success_symbol = "[](blue) [❯](bold green)";
        error_symbol = "[](blue) [❯](bold red)";
      };
      username = {
        format = "[$user](bold purple)";
      };
      hostname = {
        style = "bold blue";
        format = "[$ssh_symbol](bold bright-yellow)[$hostname]($style):";
      };
      git_branch = {
        style = "bold bright-yellow";
        format = "[$symbol$branch(:$remote_branch)]($style) ";
      };
      git_status = {
        style = "bold #${purple}";
      };
      cmd_duration = {
        format = "[$duration]($style) ";
      };
      sudo = {
        disabled = false;
        symbol = " ";
        style = "bold purple";
        format = "[$symbol]($style)";
      };
      python = {
        style = "bold blue";
        format = "[\${symbol}\${pyenv_prefix}(\${version} )(\($virtualenv\) )]($style)";
      };
      nix_shell = {
        format = "[$symbol$state( \($name\))]($style) ";
      };
    };
  };
}
