{ lib, ... }:
{
  # Only in sway:
  systemd.user.services.avizo = {
    Unit = {
      PartOf = lib.mkForce [ "sway-session.target" ];
      After = lib.mkForce [ "sway-session.target" ];
    };
    Install = {
      WantedBy = lib.mkForce [ "sway-session.target" ];
    };
  };

  services.avizo = {
    enable = true;
  };
}
