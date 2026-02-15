{ pkgs, ... }:
{
  home.packages = with pkgs; [
    xdg-desktop-portal-wlr
    slurp
    grim
  ];

  systemd.user.services.xdg-desktop-portal-wlr = {
    Unit = {
      Description = "xdg-desktop-portal-wlr";
      PartOf = [ "sway-session.target" ];
      After = [ "sway-session.target" ];
    };

    Service = {
      Type = "dbus";
      BusName = "org.freedesktop.impl.portal.desktop.wlr";
      ExecStart = "${pkgs.xdg-desktop-portal-wlr}/libexec/xdg-desktop-portal-wlr";
      Restart = "on-failure";
    };

    Install = {
      WantedBy = [ "sway-session.target" ];
    };
  };
}
