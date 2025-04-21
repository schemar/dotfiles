#!/usr/bin/env lua

local catppuccin_latte = {
  rosewater = "#dc8a78",
  flamingo = "#dd7878",
  pink = "#ea76cb",
  mauve = "#8839ef",
  red = "#d20f39",
  maroon = "#e64553",
  peach = "#fe640b",
  yellow = "#df8e1d",
  green = "#40a02b",
  teal = "#179299",
  sky = "#04a5e5",
  sapphire = "#209fb5",
  blue = "#1e66f5",
  lavender = "#7287fd",
  text = "#4c4f69",
  subtext1 = "#5c5f77",
  subtext0 = "#6c6f85",
  overlay2 = "#7c7f93",
  overlay1 = "#8c8fa1",
  overlay0 = "#9ca0b0",
  surface2 = "#acb0be",
  surface1 = "#bcc0cc",
  surface0 = "#ccd0da",
  base = "#eff1f5",
  mantle = "#e6e9ef",
  crust = "#dce0e8",
}

local blueberry_peach_light = {
  mauve = "#6A67B4",
  pink = "#6A67B4",
  flamingo = "#A352A0",
  rosewater = "#A352A0",
  red = "#CC2C58",
  maroon = "#CC2C58",
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

local files = {
  "./configs/zsh/.zshrc",
  "./configs/zsh/config/peach-zsh-syntax-highlighting.zsh",
}

for _, path in pairs(files) do
  local file = io.open(path, "r")
  if file then
    local content = file:read("*all")
    file:close()

    for key, value in pairs(catppuccin_latte) do
      content = content:gsub(value, blueberry_peach_light[key])
    end

    file = io.open(path, "w")
    if file then
      file:write(content)
      file:close()
    end
  end
end
