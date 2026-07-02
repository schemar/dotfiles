{
  lib,
  pkgs,
  ...
}:
{
  targets.genericLinux.enable = true;

  # For some reason, the scaling in wayland makes the fonts way bigger. Adjusting:
  programs.ghostty.settings."font-size" = lib.mkForce 11.0;

  # Uses home-manager standalone module on debian linux:
  imports = [
    ../../home/standalone.nix
    ../../home
    ./home-desktop.nix
    {
      home.packages = with pkgs; [
        nh

        gh
        prek

        monaspace
        nerd-fonts.symbols-only
      ];

      programs.ghostty.settings.window-decoration = "auto";
      programs.ghostty.settings.window-theme = "dark";

      # Disable the packages that are managed by "parent fedora":
      programs.ghostty.package = null;
      programs.ghostty.systemd.enable = false;
      programs.swaylock.package = null;
      programs.wezterm.package = null;
      programs.fuzzel.package = null;
      wayland.windowManager.sway.package = null;
      services.mako.package = null;
      programs.waybar.package = pkgs.emptyDirectory;
    }
  ];
}
