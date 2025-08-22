{ ... }:
{
  programs.mise = {
    enable = true;
    enableZshIntegration = true;

    settings = {
      auto_install = true;
    };

    globalConfig = {
      tools = {
        node = "22.18";
      };
    };
  };
}
