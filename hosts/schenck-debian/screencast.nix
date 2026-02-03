{ pkgs, ... }:
{
  home.packages = with pkgs; [
    xdg-desktop-portal-wlr
    slurp
    grim
  ];

  home.file.".config/systemd/user/xdg-desktop-portal-wlr.service".text = # ini
    ''
      [Unit]
      Description=xdg-desktop-portal-wlr
      PartOf=graphical-session.target
      After=graphical-session.target

      [Service]
      Type=dbus
      BusName=org.freedesktop.impl.portal.desktop.wlr
      ExecStart=${pkgs.xdg-desktop-portal-wlr}/libexec/xdg-desktop-portal-wlr
      Restart=on-failure

      [Install]
      WantedBy=graphical-session.target
    '';
}
