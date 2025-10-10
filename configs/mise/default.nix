{ ... }:
{
  programs.mise = {
    enable = true;
    enableZshIntegration = true;

    settings = {
      auto_install = true;
    };
  };
}
