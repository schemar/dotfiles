{
  inputs,
  lib,
  isDarwin,
  ...
}:
{
  programs.ghostty = {
    enable = true;
    # Darwin manages ghostty installation with homebrew:
    package = lib.mkIf isDarwin null;
    settings = {
      theme = "dark:blueberry_peach_dark,light:blueberry_peach_light";

      font-family = [
        "MonoLisaCode"
        "Symbols Nerd Font Mono"
        "Noto Color Emoji"
      ];

      font-size = 13.0;

      # -- != := === >= >- >=> |-> -> <$> </> #[ |||> |= ~@ ~- ~=
      # |> <|> =/= =:=

      font-feature = [
        "ss03"
        "ss13"
        "ss14"
        "cv01"
        "cv02"
        "cv03"
        "cv04"
        "cv05"
        "cv06"
        "cv07"
        "cv08"
        "cv09"
        "cv10"
        "cv11"
        "cv12"
      ];

      quit-after-last-window-closed = true;
      confirm-close-surface = false;

      bell-features = "no-title";

      window-decoration = "none";

      window-padding-balance = true;
      window-padding-x = 3;
      window-padding-y = 1;
      window-padding-color = "extend";

      macos-option-as-alt = true;
    };
  };

  xdg.configFile."ghostty/themes/blueberry_peach_dark" = {
    source = "${inputs.blueberry-peach}/ports/ghostty/blueberry_peach_dark";
  };
  xdg.configFile."ghostty/themes/blueberry_peach_light" = {
    source = "${inputs.blueberry-peach}/ports/ghostty/blueberry_peach_light";
  };
}
