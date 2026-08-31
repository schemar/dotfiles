{ lib, pkgs, ... }:
{
  programs.helix = {
    enable = true;
    settings = {
      theme = "base16_terminal";
    };
    languages = {
      language-server.nixd.command = "${lib.getExe pkgs.nixd}";
      language-server.elp = {
        command = "${pkgs.erlang-language-platform}/bin/elp";
        args = ["server"];
      };
    };
  };
}
