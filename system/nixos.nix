{ username, ... }:
{
  # NixOS-specific system configuration

  # Create the user:
  users.users.${username} = {
    isNormalUser = true;
    # "wheel" for sudo:
    extraGroups = [ "wheel" ];
  };

  # Passwordless sudo for user:
  security.sudo.extraRules = [
    {
      users = [ username ];
      commands = [
        {
          command = "ALL";
          options = [ "NOPASSWD" ];
        }
      ];
    }
  ];
}
