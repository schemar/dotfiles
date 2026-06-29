{ pkgs, ... }:
{
  services.kanshi = {
    enable = true;

    # Get criteria with swaymsg -t get_outputs
    settings = [
      {
        profile.name = "undocked";
        profile.outputs = [
          {
            criteria = "eDP-1";
            scale = 2.0;
            status = "enable";
          }
        ];
      }
      {
        profile.name = "home_office";
        profile.outputs = [
          {
            criteria = "eDP-1";
            status = "disable";
          }
          {
            criteria = "Dell Inc. DELL S2722QC 5Q7VLD3";
            scale = 2.0;
            status = "enable";
          }
        ];
      }
      {
        profile.name = "entelios_office";
        profile.outputs = [
          {
            criteria = "eDP-1";
            status = "disable";
          }
          {
            criteria = "Dell Inc. DELL U2412M Y1H5T27508CL";
            scale = 1.0;
            status = "enable";
          }
        ];
      }
      {
        profile.name = "entelios_office_2";
        profile.outputs = [
          {
            criteria = "eDP-1";
            status = "disable";
          }
          {
            criteria = "Dell Inc. DELL U2414H 292K477303PL";
            scale = 1.0;
            status = "enable";
          }
        ];
      }
    ];
  };
  services.podman.enable = true;

  home.packages = with pkgs; [
    gh
    prek

    podman-compose

    watchexec

    remmina # RDP
  ];
}
