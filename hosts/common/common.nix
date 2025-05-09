{ ... }:
{
  # Use user-level ZSH provided by home-manager config:
  environment.shells = [ "/etc/profiles/per-user/schemar/bin/zsh" ];
  users.users.schemar.shell = "/etc/profiles/per-user/schemar/bin/zsh";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  # environment.systemPackages = with pkgs; [ ];
}
