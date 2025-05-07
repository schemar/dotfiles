{ config, ... }:
{
  home.file.".ssh/config".source = ./config;
}
