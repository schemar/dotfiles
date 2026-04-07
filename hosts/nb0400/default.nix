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
    {
      fonts.fontconfig.enable = true;
      home.packages = with pkgs; [
        prek
        monaspace
        nerd-fonts.symbols-only
      ];
      programs.ghostty.settings.window-decoration = "auto";
      programs.ghostty.settings.window-theme = "dark";
      services.podman.enable = true;
    }
  ];
}
