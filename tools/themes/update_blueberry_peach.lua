#!/usr/bin/env lua

local blueberry_peach_light = {
  mauve = "#6A67B4",
  pink = "#6A67B4",
  flamingo = "#A352A0",
  rosewater = "#A352A0",
  red = "#C53F64",
  maroon = "#C53F64",
  yellow = "#8A7400",
  peach = "#AC591C",
  green = "#288043",
  teal = "#007E7D",
  sky = "#007E7D",
  sapphire = "#007E7D",
  blue = "#1675AB",
  lavender = "#1675AB",
  text = "#5B5B5B",
  subtext1 = "#908A84",
  subtext0 = "#908A84",
  overlay2 = "#707070",
  overlay1 = "#646464",
  overlay0 = "#585858",
  surface2 = "#9C8282",
  surface1 = "#e8dfd4",
  surface0 = "#f1ece5",
  base = "#faf4ed",
  mantle = "#FFFAF0",
  crust = "#FFFAF0",
}

local template_files = {
  {
    source = "./tools/themes/templates/bat/blueberry_peach_light.tmTheme",
    target = "./configs/bat/themes/blueberry_peach_light.tmTheme",
  },
  {
    source = "./tools/themes/templates/fzf/blueberry_peach_light.zsh",
    target = "./configs/zsh/config/blueberry_peach_light-fzf-colors.zsh",
  },
  {
    source = "./tools/themes/templates/nvim/blueberry_peach_light.lua",
    target = "./configs/neovim/lua/schemar/config/themes/blueberry_peach_light.lua",
  },
  {
    source = "./tools/themes/templates/tmux/blueberry_peach_light.conf",
    target = "./configs/tmux/config/blueberry_peach_light.conf",
  },
  {
    source = "./tools/themes/templates/wezterm/blueberry_peach_light.lua",
    target = "./configs/wezterm/blueberry_peach_light.lua",
  },
  {
    source = "./tools/themes/templates/zsh_syntax_highlighting/blueberry_peach_light.zsh",
    target = "./configs/zsh/config/blueberry_peach_light-syntax-highlighting.zsh",
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

  for key, value in pairs(blueberry_peach_light) do
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
