{
  inputs,
  isDarwin,
  ...
}:
{
  xdg.configFile."ghostty/config".text = # ini{
    ''
      theme = dark:blueberry_peach_dark,light:blueberry_peach_light

      font-family = Monaspace Neon
      font-family = Symbols Nerd Font Mono

      font-size = 13

      font-feature = "calt=1"
      font-feature = "dlig=1"
      font-feature = "cv01=2"
      font-feature = "ss01=1"
      font-feature = "ss02=1"
      font-feature = "ss03=1"
      font-feature = "ss04=1"
      font-feature = "ss05=1"
      font-feature = "ss06=1"
      font-feature = "ss07=1"
      font-feature = "ss08=1"
      font-feature = "ss09=1"
      font-feature = "ss10=1"

      quit-after-last-window-closed
      confirm-close-surface = false

      window-decoration = ${if isDarwin then "none" else "auto"}

      window-padding-balance = true
      window-padding-x = 3
      window-padding-y = 1
      window-padding-color = extend

      macos-option-as-alt = true
    '';

  xdg.configFile."ghostty/themes/blueberry_peach_dark" = {
    source = "${inputs.blueberry-peach}/ports/ghostty/blueberry_peach_dark";
  };
  xdg.configFile."ghostty/themes/blueberry_peach_light" = {
    source = "${inputs.blueberry-peach}/ports/ghostty/blueberry_peach_light";
  };
}
