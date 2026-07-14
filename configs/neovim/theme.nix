{ inputs, ... }:
{
  xdg.dataFile."nvim/site/pack/themes/start/blueberry-peach" = {
    source = "${inputs.blueberry-peach}/ports/neovim";
  };
  programs.nixvim = {
    opts = {
      # [[ Theme ]]
      syntax = "ON"; # str: Allow syntax highlighting
      termguicolors = true; # bool: If term supports ui color then enable
      cursorline = true; # bool: Highlight current line
      # listchars = "space:·,tab:>~,trail:~,extends:>,precedes:<,eol:󰌑"
      listchars = "tab:~~,trail:~";
      list = true;

    };
    colorscheme = "blueberry-peach";
    plugins.lualine.settings.options.theme = "blueberry-peach";
  };
}
