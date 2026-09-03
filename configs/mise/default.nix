{ ... }:
{
  programs.mise = {
    enable = true;

    globalConfig = {
      settings = {
        auto_install = true;
      };
    };
  };
}
