{
  lib,
  pkgs,
  username,
  ...
}:
{
  virtualisation = {
    podman = {
      enable = true;
      # Have a "docker" command that runs podman.
      # Useful for projects that have a justfile with docker commands.
      dockerCompat = true;
    };

    libvirtd = {
      enable = true;
      qemu = {
        # TPM Emulation
        swtpm.enable = true;
      };
    };
  };

  programs.virt-manager.enable = true;
  environment.systemPackages = with pkgs; [
    # Install pkgs.swtpm system-wide for use in virt-manager:
    swtpm
    # Required for DNS and DHCP inside the guest:
    dnsmasq
  ];

  users.users.${username}.extraGroups = [ "libvirtd" ];
}
