{ username, ... }:
{
  # Interactive completion is initialized by Home Manager.
  programs.zsh.enableCompletion = false;

  # Make completion definitions from system packages visible to the
  # user-level compinit invocation.
  environment.pathsToLink = [ "/share/zsh" ];

  home-manager.users.${username} = {
    imports = [
      ../../home/default
    ];
  };
}
