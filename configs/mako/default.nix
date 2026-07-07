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
      background-color=#0B0A0F
      text-color=#A2A2A9
      progress-color=over #A19DD4FF

      [mode=light]
      border-color=#6A67B4
      background-color=#FAF4ED
      text-color=#706F7A
      progress-color=over #6A67B4
    '';

  services.mako = {
    enable = true;
  };

}
