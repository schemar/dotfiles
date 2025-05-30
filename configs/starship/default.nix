{ ... }:
{
  programs.starship = {
    enable = true;
    enableZshIntegration = true;

    settings = {
      username = {
        style_root = "fg:red";
        style_user = "fg:purple";
      };
      hostname = {
        style = "fg:purple";
      };
      character = {
        success_symbol = "[>](blue)";
        error_symbol = "[>](red)";
        vicmd_symbol = "[<](blue)";
      };
      cmd_duration = {
        format = "took [$duration](fg:yellow) ";
      };
      directory = {
        style = "fg:blue";
        truncate_to_repo = false;
      };
      aws = {
        style = "fg:green";
        symbol = " ";
        disabled = true;
      };
      gcloud = {
        style = "fg:green";
        symbol = " ";
        disabled = true;
      };
      battery = {
        #style = "fg:blue";
        full_symbol = "󰁹 ";
        charging_symbol = "󰂄 ";
        discharging_symbol = "󰂃 ";
      };
      conda = {
        style = "fg:cyan";
        symbol = " ";
      };
      docker_context = {
        style = "fg:green";
        symbol = " ";
      };
      elixir = {
        style = "fg:cyan";
        symbol = " ";
      };
      elm = {
        style = "fg:cyan";
        symbol = " ";
      };
      git_branch = {
        # Use tmux status right instead
        disabled = true;
        style = "fg:yellow";
        symbol = " ";
      };
      git_commit = {
        style = "fg:yellow";
      };
      git_state = {
        style = "fg:yellow";
      };
      git_status = {
        style = "fg:yellow";
        modified = "";
        deleted = "";
        untracked = "U";
        staged = "S";
        renamed = "➜";
      };
      golang = {
        style = "fg:cyan";
        symbol = " ";
      };
      hg_branch = {
        style = "fg:purple";
        symbol = " ";
      };
      java = {
        style = "fg:cyan";
        symbol = " ";
      };
      julia = {
        style = "fg:cyan";
        symbol = " ";
      };
      memory_usage = {
        style = "fg:yellow";
        symbol = "󰍛 ";
      };
      nim = {
        style = "fg:cyan";
        symbol = "󰆥 ";
      };
      nix_shell = {
        style = "fg:blue";
        symbol = " ";
      };
      nodejs = {
        style = "fg:cyan";
        symbol = " ";
      };
      package = {
        style = "fg:blue";
        symbol = "󰏗 ";
      };
      php = {
        style = "fg:cyan";
        symbol = " ";
      };
      pulumi = {
        style = "fg:purple";
        symbol = " ";
        disabled = true;
      };
      python = {
        style = "fg:cyan";
        symbol = " ";
      };
      ruby = {
        style = "fg:cyan";
        symbol = " ";
      };
      rust = {
        style = "fg:cyan";
        symbol = " ";
      };
      time = {
        style = "fg:purple";
        disabled = false;
      };
    };
  };
}
