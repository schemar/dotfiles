{ ... }:
{
  programs.lsd = {
    enable = true;

    colors = {
      user = "yellow";
      group = "yellow";
      size = {
        none = "dark_yellow";
        small = "yellow";
        medium = "yellow";
        large = "yellow";
      };
      date = {
        hour-old = "green";
        day-old = "green";
        older = "dark_green";
      };
    };

    # I manage my own aliases:
    enableZshIntegration = false;
  };
}
