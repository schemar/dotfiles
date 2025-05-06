{ ... }: {
  programs.fzf = {
    enable = true;

    defaultCommand = "fd --type f --hidden --exclude .git/";

    fileWidgetOptions = [
      "--preview 'bat --style=numbers,changes --color=always {} --theme=blueberry_peach_$(~/.config/current_theme)'"
    ];

    enableZshIntegration = true;
  };
}
