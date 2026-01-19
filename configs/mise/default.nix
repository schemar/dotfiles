{ ... }:
{
  programs.mise = {
    enable = true;
    enableZshIntegration = true;

    globalConfig = {
      settings = {
        auto_install = true;
      };
      tools = {
        node = "24.11.1";
      };
    };
  };
}
