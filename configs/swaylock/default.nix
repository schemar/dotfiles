{ ... }:
{
  programs.swaylock = {
    enable = true;
    # Disabled due to broken PAM stack with Debian. Instead:
    # sudo apt install swaylock
    package = null;

    settings = {
      color = "191724";
    };
  };
}
