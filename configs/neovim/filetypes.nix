{ ... }:
{
  programs.nixvim.files = {
    "ftplugin/gdscript.lua" = {
      opts = {
        expandtab = false;
      };
    };
  };
}
