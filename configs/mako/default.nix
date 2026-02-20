{ pkgs, lib, ... }:
{
  # Only in sway:
  systemd.user.services.mako = {
    Unit = {
      Description = "Lightweight Wayland notification daemon";
      Documentation = "man:mako(1)";
      PartOf = lib.mkForce [ "sway-session.target" ];
      After = lib.mkForce [ "sway-session.target" ];
    };
    Service = {
      Type = "dbus";
      BusName = "org.freedesktop.Notifications";
      ExecCondition = "/bin/sh -c '[ -n \"$WAYLAND_DISPLAY\" ]'";
      ExecStart = "${pkgs.mako}/bin/mako";
      ExecReload = "${pkgs.mako}/bin/makoctl reload";
    };
    Install = {
      WantedBy = lib.mkForce [ "sway-session.target" ];
    };
  };

  services.mako = {
    enable = true;

    settings = {
      font = "Open Sans 11";
      background-color = "#0B0A0F";
      text-color = "#A2A2A9";
      width = "600";
      height = "80";
      margin = "6";
      border-size = "1";
      border-color = "#A19DD4FF";
      border-radius = "0";
      progress-color = "over #A19DD4FF";
      padding = "8";
    };
  };

}
