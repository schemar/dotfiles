{ ... }:
{
  # The platform the configuration will be used on:
  nixpkgs.hostPlatform = "aarch64-darwin";

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 5;

  # Required by home-manager:
  users.users.schemar.home = /Users/schemar;

  homebrew = {
    enable = true;
    onActivation = {
      cleanup = "zap";
    };

    taps = [
      "qmk/qmk"
      "osx-cross/arm" # required by qmk/qmk/qmk
      "osx-cross/avr" # required by qmk/qmk/qmk
    ];

    brews = [
      "qmk/qmk/qmk"
    ];

    casks = [
      "anki"
      "ghostty"
      "gimp"
      "raycast"
      "wezterm"
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

  # Move icons in menu bar closer together:
  system.defaults.NSGlobalDomain.NSStatusItemSpacing = 10;
  system.defaults.NSGlobalDomain.NSStatusItemSelectionPadding = 8;

  # Don't show these icons in the menu bar so that other icons do not disappear
  # behind the notch:
  # (assuming `null` means "auto")
  system.defaults.controlcenter.AirDrop = false;
  system.defaults.controlcenter.Bluetooth = false;
  system.defaults.controlcenter.Display = false;
  system.defaults.controlcenter.FocusModes = false;
  system.defaults.controlcenter.NowPlaying = false;
  system.defaults.controlcenter.Sound = false;

  system.defaults.dock.autohide = true;
  system.defaults.dock.autohide-delay = 0.0;
  system.defaults.dock.autohide-time-modifier = 0.2;
  system.defaults.dock.tilesize = 40;
  system.defaults.dock.largesize = 40;
  system.defaults.dock.magnification = false;
  system.defaults.dock.orientation = "right";
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

  system.defaults.universalaccess.mouseDriverCursorSize = 2.0;
}
