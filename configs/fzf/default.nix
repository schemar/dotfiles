{ ... }:
{
  programs.fzf = {
    enable = true;

    defaultCommand = "fd --type f --hidden --exclude .git/";
  };
}
