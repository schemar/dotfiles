{ pkgs, ...} :{
  # Enable alternative shell support.
  programs.zsh.enable = true;
  environment.shells = with pkgs; [ zsh ];
  users.users.schemar.shell = pkgs.zsh;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    delta
    just
    tmux
  ];
}
