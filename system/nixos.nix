{ ... }:
{
  # NixOS-specific system configuration

  # Create the schemar user:
  users.users.schemar = {
    isNormalUser = true;
    # "wheel" for sudo:
    extraGroups = [ "wheel" ];
  };

  # Passwordless sudo for schemar:
  security.sudo.extraRules = [
    {
      users = [ "schemar" ];
      commands = [
        {
          command = "ALL";
          options = [ "NOPASSWD" ];
        }
      ];
    }
  ];
}
