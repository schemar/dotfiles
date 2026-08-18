{ inputs, ... }:
{
  xdg.configFile."mako/config".text = # ini
    ''
      font=MonoLisaText 11
      width=600
      height=80
      outer-margin=12,12,0,0
      margin=0,0,6,0
      border-size=1
      border-radius=0
      padding=8

      [mode=dark]
      ${builtins.readFile "${inputs.blueberry-peach}/ports/mako/blueberry_peach_dark"}

      [mode=light]
      ${builtins.readFile "${inputs.blueberry-peach}/ports/mako/blueberry_peach_light"}
    '';

  services.mako = {
    enable = true;
  };

}
