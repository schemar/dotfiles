{
  inputs,
  isDarwin,
  pkgs,
  ...
}:
{
  programs.ghostty = {
    enable = true;
    # Darwin manages ghostty installation with homebrew:
    package = if isDarwin then null else pkgs.ghostty;
    settings = {
      theme = "dark:blueberry_peach_dark,light:blueberry_peach_light";

      "font-family" = [
        "Monaspace Neon"
        "Symbols Nerd Font Mono"
      ];

      "font-size" = 13;

      "font-feature" = [
        "calt=1"
        "dlig=1"
        "cv01=2"
        "ss01=1"
        "ss02=1"
        "ss03=1"
        "ss04=1"
        "ss05=1"
        "ss06=1"
        "ss07=1"
        "ss08=1"
        "ss09=1"
        "ss10=1"
      ];

      "quit-after-last-window-closed" = true;
      "confirm-close-surface" = false;

      "window-decoration" = if isDarwin then "none" else "auto";

      "window-padding-balance" = true;
      "window-padding-x" = 3;
      "window-padding-y" = 1;
      "window-padding-color" = "extend";

      "macos-option-as-alt" = true;
    };
  };

  xdg.configFile."ghostty/themes/blueberry_peach_dark" = {
    source = "${inputs.blueberry-peach}/ports/ghostty/blueberry_peach_dark";
  };
  xdg.configFile."ghostty/themes/blueberry_peach_light" = {
    source = "${inputs.blueberry-peach}/ports/ghostty/blueberry_peach_light";
  };
}
