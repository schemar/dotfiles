{ ... }: {
  programs.bat = {
    enable = true;

    config = {
      style = "numbers,changes";
      theme = "blueberry_peach_light";
    };

    themes = {
      blueberry_peach_light = {
        src = ./themes;
        file = "blueberry_peach_light.tmTheme";
      };
      blueberry_peach_dark = {
        src = ./themes;
        file = "blueberry_peach_dark.tmTheme";
      };
    };
  };
}
