{ ... }:
{
  xdg.configFile."mako/config".text = # ini
    ''
      font=MonoLisaText 11
      width=600
      height=80
      outer-margin=12,6,0,0
      margin=0,0,6,0
      border-size=1
      border-radius=0
      padding=8

      [mode=dark]
      border-color=#A19DD4FF
      background-color=#37363E
      text-color=#A2A2A9
      progress-color=over #A19DD4FF

      [mode=light]
      border-color=#6E3B96
      background-color=#DDC8B6
      text-color=#57534C
      progress-color=over #6E3B96
    '';

  services.mako = {
    enable = true;
  };

}
