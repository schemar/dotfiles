{ inputs, ... }:
{
  programs.bat = {
    enable = true;

    config = {
      style = "numbers,changes";
    };

    themes = {
      blueberry_peach_light = {
        src = "${inputs.blueberry-peach}/ports/bat/blueberry_peach_light.tmTheme";
      };
      blueberry_peach_dark = {
        src = "${inputs.blueberry-peach}/ports/bat/blueberry_peach_dark.tmTheme";
      };
    };
  };
}
