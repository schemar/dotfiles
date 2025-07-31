{ ... }:
{
  users.users.schemar = {
    isNormalUser = true;
    # "wheel" for sudo:
    extraGroups = [ "wheel" ];
  };

  security.sudo.extraRules = [
    {
      users = [ "schemar" ];
      commands = [
        {
          # passwordless sudo:
          command = "ALL";
          options = [ "NOPASSWD" ];
        }
      ];
    }
  ];
}
