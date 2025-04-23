#!/usr/bin/env lua

local blueberry_peach_light = {
  red = "#C34165",
  maroon = "#C34165",
  yellow = "#8A7400",
  peach = "#AC591C",
  green = "#288043",
  teal = "#007E7D",
  sky = "#007E7D",
  sapphire = "#007E7D",
  blue = "#1675AB",
  lavender = "#1675AB",
  mauve = "#6A67B4",
  pink = "#6A67B4",
  flamingo = "#A352A0",
  rosewater = "#A352A0",
  text = "#706F7A",
  subtext1 = "#757480",
  subtext0 = "#757480",
  overlay2 = "#797985",
  overlay1 = "#7E7D8A",
  overlay0 = "#84828F",
  surface2 = "#9C8282",
  surface1 = "#EBDFD3",
  surface0 = "#EBDFD3",
  base = "#FAF4ED",
  mantle = "#FCF9F5",
  crust = "#FCF9F5",
  id = "light",
  name = "Blueberry Peach Light",
}

local blueberry_peach_dark = {
  red = "#BF7789",
  maroon = "#BF7789",
  yellow = "#9D8C30",
  peach = "#AB8465",
  green = "#659774",
  teal = "#519897",
  sky = "#519897",
  sapphire = "#519897",
  blue = "#6292AD",
  lavender = "#6292AD",
  mauve = "#8A87B9",
  pink = "#8A87B9",
  flamingo = "#B079AD",
  rosewater = "#B079AD",
  text = "#8A8A9E",
  subtext1 = "#878794",
  subtext0 = "#878794",
  overlay2 = "#7D7D7D",
  overlay1 = "#808084",
  overlay0 = "#84848C",
  surface2 = "#7C7992",
  surface1 = "#2D2B37",
  surface0 = "#2D2B37",
  base = "#191724",
  mantle = "#0B0A0F",
  crust = "#0B0A0F",
  id = "dark",
  name = "Blueberry Peach Dark",
}

local blueberry_peach_dark_dimmed = {
  red = "#BF657B",
  maroon = "#BF657B",
  yellow = "#91812C",
  peach = "#A67753",
  green = "#578D67",
  teal = "#278F8E",
  sky = "#278F8E",
  sapphire = "#278F8E",
  blue = "#5188A6",
  lavender = "#5188A6",
  mauve = "#7E7ABA",
  pink = "#7E7ABA",
  flamingo = "#A370A1",
  rosewater = "#A370A1",
  text = "#7F7F91",
  subtext1 = "#797985",
  subtext0 = "#797985",
  overlay2 = "#6E6E6E",
  overlay1 = "#717175",
  overlay0 = "#76767D",
  surface2 = "#6A677E",
  surface1 = "#29263A",
  surface0 = "#29263A",
  base = "#191724",
  mantle = "#0B0A0F",
  crust = "#0B0A0F",
  id = "dark-dimmed",
  name = "Blueberry Peach Dark Dimmed",
}

local template_files = {
  {
    source = "./tools/themes/templates/bat/blueberry_peach.tmTheme",
    target = "./configs/bat/themes/blueberry_peach_light.tmTheme",
    colors = blueberry_peach_light,
  },
  {
    source = "./tools/themes/templates/fzf/blueberry_peach.zsh",
    target = "./configs/zsh/config/blueberry_peach_light-fzf-colors.zsh",
    colors = blueberry_peach_light,
  },
  {
    source = "./tools/themes/templates/nvim/blueberry_peach.lua",
    target = "./configs/neovim/lua/schemar/config/themes/blueberry_peach_light.lua",
    colors = blueberry_peach_light,
  },
  {
    source = "./tools/themes/templates/tmux/blueberry_peach.conf",
    target = "./configs/tmux/config/blueberry_peach_light.conf",
    colors = blueberry_peach_light,
  },
  {
    source = "./tools/themes/templates/wezterm/blueberry_peach.toml",
    target = "./configs/wezterm/colors/blueberry_peach_light.toml",
    colors = blueberry_peach_light,
  },
  {
    source = "./tools/themes/templates/zsh_syntax_highlighting/blueberry_peach.zsh",
    target = "./configs/zsh/config/blueberry_peach_light-syntax-highlighting.zsh",
    colors = blueberry_peach_light,
  },
  {
    source = "./tools/themes/templates/bat/blueberry_peach.tmTheme",
    target = "./configs/bat/themes/blueberry_peach_dark.tmTheme",
    colors = blueberry_peach_dark,
  },
  {
    source = "./tools/themes/templates/fzf/blueberry_peach.zsh",
    target = "./configs/zsh/config/blueberry_peach_dark-fzf-colors.zsh",
    colors = blueberry_peach_dark,
  },
  {
    source = "./tools/themes/templates/nvim/blueberry_peach.lua",
    target = "./configs/neovim/lua/schemar/config/themes/blueberry_peach_dark.lua",
    colors = blueberry_peach_dark,
  },
  {
    source = "./tools/themes/templates/tmux/blueberry_peach.conf",
    target = "./configs/tmux/config/blueberry_peach_dark.conf",
    colors = blueberry_peach_dark,
  },
  {
    source = "./tools/themes/templates/wezterm/blueberry_peach.toml",
    target = "./configs/wezterm/colors/blueberry_peach_dark.toml",
    colors = blueberry_peach_dark,
  },
  {
    source = "./tools/themes/templates/zsh_syntax_highlighting/blueberry_peach.zsh",
    target = "./configs/zsh/config/blueberry_peach_dark-syntax-highlighting.zsh",
    colors = blueberry_peach_dark,
  },
}

for _, template_file in pairs(template_files) do
  local file = io.open(template_file.source, "r")
  if file == nil then
    print("Error: Could not open source file " .. template_file.source)
    return
  end

  local content = file:read("*all")
  file:close()

  for key, value in pairs(template_file.colors) do
    content = content:gsub("{{" .. key .. "}}", value)
  end

  file = io.open(template_file.target, "w")
  if file == nil then
    print("Error: Could not open target file " .. template_file.target)
    return
  end
  file:write(content)
  file:close()
end
