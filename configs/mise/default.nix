{ ... }:
{
  programs.mise = {
    enable = true;
    enableZshIntegration = true;

    globalConfig = {
      tools = {
        node = "lts";
      };
    };
  };
}
