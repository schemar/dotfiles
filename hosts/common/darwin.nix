{ ... }:
{
  # The platform the configuration will be used on:
  nixpkgs.hostPlatform = "aarch64-darwin";

  # Required by home-manager:
  users.users.schemar.home = /Users/schemar;

  homebrew = {
    enable = true;
    onActivation = {
      cleanup = "zap";
    };

    casks = [
      "gimp"
      "raycast"
      "wezterm"

      # Could probably be done with nix-darwin as well:
      # https://nix-darwin.github.io/nix-darwin/manual/index.html#opt-fonts.packages
      "font-monaspace"
      "font-symbols-only-nerd-font"
    ];
  };

  # Add ability to use TouchID for sudo authentication in terminal:
  security.pam.services.sudo_local.touchIdAuth = true;
  # This fixes Touch ID for sudo not working inside tmux and screen:
  security.pam.services.sudo_local.reattach = true;

  system.defaults.CustomUserPreferences = {
    NSGlobalDomain = {
      # Disable mouse acceleration:
      "com.apple.mouse.linear" = true;
    };
  };

  # Mouse speed:
  system.defaults.".GlobalPreferences"."com.apple.mouse.scaling" = 0.6875;
  # Mode 3 enables full keyboard control:
  system.defaults.NSGlobalDomain.AppleKeyboardUIMode = 3;
  # Jump to the spot that’s clicked on the scroll bar:
  system.defaults.NSGlobalDomain.AppleScrollerPagingBehavior = true;
  # Finder:
  system.defaults.NSGlobalDomain.AppleShowAllExtensions = true;
  system.defaults.NSGlobalDomain.AppleShowAllFiles = true;
  # Keyboard:
  system.defaults.NSGlobalDomain.InitialKeyRepeat = 15;
  system.defaults.NSGlobalDomain.KeyRepeat = 2;
  system.defaults.NSGlobalDomain.NSAutomaticDashSubstitutionEnabled = false;

  system.defaults.NSGlobalDomain.NSNavPanelExpandedStateForSaveMode = true;
  system.defaults.NSGlobalDomain.NSNavPanelExpandedStateForSaveMode2 = true;
  system.defaults.NSGlobalDomain.NSTableViewDefaultSizeMode = 2;
  system.defaults.NSGlobalDomain."com.apple.sound.beep.volume" = 0.000;

  system.defaults.WindowManager.StandardHideDesktopIcons = true;
  system.defaults.WindowManager.StandardHideWidgets = true;

  system.defaults.controlcenter.BatteryShowPercentage = true;

  system.defaults.dock.autohide = true;
  system.defaults.dock.autohide-delay = 0.0;
  system.defaults.dock.autohide-time-modifier = 0.1;
  system.defaults.dock.tilesize = 40;
  system.defaults.dock.largesize = 56;
  system.defaults.dock.magnification = true;
  system.defaults.dock.orientation = "bottom";
  system.defaults.dock.show-recents = false;
  system.defaults.dock.showhidden = true;
  system.defaults.dock.mru-spaces = false;

  # Disable hot corners:
  system.defaults.dock.wvous-bl-corner = 1;
  system.defaults.dock.wvous-br-corner = 1;
  system.defaults.dock.wvous-tl-corner = 1;
  system.defaults.dock.wvous-tr-corner = 1;

  system.defaults.finder.AppleShowAllExtensions = true;
  system.defaults.finder.AppleShowAllFiles = true;
  system.defaults.finder.CreateDesktop = false;
  system.defaults.finder.FXPreferredViewStyle = "Nlsv";
  system.defaults.finder.NewWindowTarget = "Home";
  system.defaults.finder.ShowPathbar = true;
  system.defaults.finder.ShowStatusBar = true;
  system.defaults.finder._FXShowPosixPathInTitle = true;
  system.defaults.finder._FXSortFoldersFirst = true;

  # Default to seach in current folder:
  system.defaults.finder.FXDefaultSearchScope = "SCcf";

  system.defaults.loginwindow.GuestEnabled = false;

  system.defaults.menuExtraClock.FlashDateSeparators = true;
  system.defaults.menuExtraClock.Show24Hour = true;

  system.defaults.screensaver.askForPassword = true;
  system.defaults.screensaver.askForPasswordDelay = 0;

  system.defaults.spaces.spans-displays = true;

  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToEscape = true;
}
