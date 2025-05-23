{ inputs, ... }:
{
  imports = [
    inputs.nixvim.homeManagerModules.nixvim
  ];

  programs.nixvim = {
    enable = true;
    defaultEditor = true;
    vimdiffAlias = true;

    colorschemes = {
      catppuccin = {
        enable = true;
        autoLoad = true;
      };
    };
  };
}
