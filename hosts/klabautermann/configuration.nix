{ ... }:
{
  imports = [
    ./hardware-configuration.nix
    ./networking.nix # generated at runtime by nixos-infect

  ];

  boot.tmp.cleanOnBoot = true;
  zramSwap.enable = true;
  networking.hostName = "klabautermann";
  networking.domain = "";
  services.openssh.enable = true;
  users.users.root.openssh.authorizedKeys.keys = [
    ''ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIChFNUALd5qGQ96dfCbPAwLq/qV1v7xOCXJlyFCwMSkV''
    ''ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDdl/KXXbpqi5ZqZf4iXigkP2XU4vXngZNEMjj1PR4GrPZnY93LlPCd5qWQXyWoTAEZJgkwIOnD9g6iRfHPC3p08sRcwqFG50Xq0zEsmjqFYgm86mL0TZ9SmiO3+obHFICG51X2BeFp0RG3VdMvgt0q9gH7iQqF5bnZFiKeYuyRJmO01uzXTj6NmcZCKuAEevlOf4/nwQ0X01QcwBYhKCriBS4kjWMhM9/LlQG9SaQtyHsiohcKFGtPALWSY8Smvwhw30ebLMj4930sAMPna9S5h5y7REum2LSCrCnIvoi3iTXvdsTd0cmW6eTRfE/ppFcaKsU14OAsARv0tX0LMiCwx0FCDVZGMJWNTzuYzgYUlY30CwDw/C2fNyETHrxDTXDb4tNCRDSU8OSg3JCAJI9s56avXkOmuGgaNpP9a/YM3Q5fEs3BbwVQn/rcZ55kMjRs5efbJzUTPz3fFlq2JK+v35gKsVdjuFhKlAjeYdltxACwRPaHgVcLdZX8CfxFCXtgNpl+qhz8/8mF5phnFianbpK11Sj0hWaCSo4q1qzjqfwcyy1DdboKDEHqSG762VxGs7LiHWP9n6SkvnT1JbU9tb05q1TD1iDxh4NBNCK2QK5nw92t5Ntoi/41CosYCtGb15bdskKLejflzeEEAEozk+q4KaUmaJ975JwuXA83JQ== martinschenck@fastmail.com''
    ''ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKgAC0Py5KkT0hrmIKcXHLGKe1/57+/0A1RjNB1BPS6i''
  ];
  system.stateVersion = "23.11";
}
