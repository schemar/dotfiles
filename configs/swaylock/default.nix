{ inputs, ... }:
{
  xdg.configFile."swaylock/config".text = ''
    ${builtins.readFile "${inputs.blueberry-peach}/ports/swaylock/blueberry_peach_dark.conf"}
    show-failed-attempts
  '';

  programs.swaylock = {
    enable = true;
  };
}
