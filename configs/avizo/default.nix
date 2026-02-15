{ pkgs, ... }:
{
  # Only in sway:
  systemd.user.services.avizo = {
    Unit = {
      Description = "Volume/backlight OSD indicator";
      Documentation = "man:avizo(1)";
      PartOf = [ "sway-session.target" ];
      After = [ "sway-session.target" ];
    };
    Service = {
      ExecStart = "${pkgs.avizo}/bin/avizo-service";
      Restart = "always";
      Type = "simple";
    };
    Install = {
      WantedBy = [ "sway-session.target" ];
    };
  };

  services.avizo = {
    enable = true;
  };
}
