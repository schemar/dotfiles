{ lib, pkgs, ... }:
{
  services.blueman-applet = {
    enable = true;
  };

  home.packages = with pkgs; [
    swayidle
    swaybg
    glib
    kdePackages.polkit-kde-agent-1

    libnotify
    wl-clipboard
    playerctl

    networkmanager
    networkmanagerapplet

    pulseaudio
    pavucontrol
    blueman

    grim
    slurp
    swappy

    wtype # Type on wayland like xdotool; used by bemoji

    nautilus # gnome file manager

    eog # eye of gnome image viewer
    gimp
    thunderbird
    obsidian
    todoist-electron
  ];

  services.udiskie.enable = true;

  gtk = {
    enable = true;
    gtk4.theme = null;
  };
  qt = {
    enable = true;
  };

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

  imports = [
    ./linux-desktop.nix
    ../configs/chromium
    ../configs/firefox
  ];

  wayland.windowManager.sway.config.startup = [
    { command = "${pkgs.kdePackages.polkit-kde-agent-1}/libexec/polkit-kde-authentication-agent-1"; }
  ];

}
