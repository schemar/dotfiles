{ ... }:
{
  # The platform the configuration will be used on:
  nixpkgs.hostPlatform = "aarch64-darwin";
  # Include unfree packages from nixpkgs:
  nixpkgs.config.allowUnfree = true;

  # Required by home-manager:
  users.users.schemar.home = /Users/schemar;
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;

    users.schemar = ../../users/schemar;
  };

  homebrew = {
    enable = true;
    onActivation = {
      cleanup = "zap";
    };

    brews = [
      # Keep coreutils here to have greadlink which I use in ZSH to get the
      # current git repo. (see zsh config)
      "coreutils"
      "neovim"
      # Used by sqlite.lua in nvim (with brew path prefix):
      "sqlite"
    ];
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

  services = {
    aerospace = {
      enable = true;
      settings = {
        mode.main.binding = {
          ctrl-alt-cmd-shift-h = "focus left";
          ctrl-alt-cmd-shift-j = "focus down";
          ctrl-alt-cmd-shift-k = "focus up";
          ctrl-alt-cmd-shift-l = "focus right";

          ctrl-1 = "workspace 1";
          ctrl-2 = "workspace 2";
          ctrl-3 = "workspace 3";
          ctrl-4 = "workspace 4";
          ctrl-5 = "workspace 5";
          ctrl-6 = "workspace 6";
          ctrl-7 = "workspace 7";
          ctrl-8 = "workspace 8";
          ctrl-9 = "workspace 9";

          ctrl-shift-1 = "move-node-to-workspace 1";
          ctrl-shift-2 = "move-node-to-workspace 2";
          ctrl-shift-3 = "move-node-to-workspace 3";
          ctrl-shift-4 = "move-node-to-workspace 4";
          ctrl-shift-5 = "move-node-to-workspace 5";
          ctrl-shift-6 = "move-node-to-workspace 6";
          ctrl-shift-7 = "move-node-to-workspace 7";
          ctrl-shift-8 = "move-node-to-workspace 8";
          ctrl-shift-9 = "move-node-to-workspace 9";

          alt-tab = "workspace-back-and-forth";

          alt-shift-semicolon = "mode service";
        };
        mode.service.binding = {
          esc = [
            "reload-config"
            "mode main"
          ];
          r = [
            "flatten-workspace-tree"
            "mode main"
          ]; # reset layout
          f = [
            "layout floating tiling"
            "mode main"
          ]; # Toggle between floating and tiling layout
          backspace = [
            "close-all-windows-but-current"
            "mode main"
          ];
          minus = "resize smart -50";
          equal = "resize smart +50";
          alt-h = [
            "move left"
            "mode main"
          ];
          alt-j = [
            "move down"
            "mode main"
          ];
          alt-k = [
            "move up"
            "mode main"
          ];
          alt-l = [
            "move right"
            "mode main"
          ];
          alt-shift-h = [
            "join-with left"
            "mode main"
          ];
          alt-shift-j = [
            "join-with down"
            "mode main"
          ];
          alt-shift-k = [
            "join-with up"
            "mode main"
          ];
          alt-shift-l = [
            "join-with right"
            "mode main"
          ];
        };
        on-window-detected = [
          {
            check-further-callbacks = true;
            "if" = {
              app-id = "com.apple.systempreferences";
            };
            run = [
              "layout floating"
            ];
          }
          {
            check-further-callbacks = false;
            "if" = {
              app-id = "com.github.wez.wezterm";
            };
            run = [
              "move-node-to-workspace 1"
            ];
          }
          {
            check-further-callbacks = false;
            "if" = {
              app-id = "com.google.Chrome";
            };
            run = [
              "move-node-to-workspace 2"
            ];
          }
          {
            check-further-callbacks = false;
            "if" = {
              app-id = "com.apple.Safari";
            };
            run = [
              "move-node-to-workspace 2"
            ];
          }
          {
            check-further-callbacks = false;
            "if" = {
              app-id = "com.todoist.mac.Todoist";
            };
            run = [
              "move-node-to-workspace 3"
            ];
          }
          {
            check-further-callbacks = false;
            "if" = {
              app-id = "com.apple.Notes";
            };
            run = [
              "move-node-to-workspace 4"
            ];
          }
          {
            check-further-callbacks = false;
            "if" = {
              app-id = "notion.id";
            };
            run = [
              "move-node-to-workspace 4"
            ];
          }
          {
            check-further-callbacks = false;
            "if" = {
              app-id = "com.tinyspeck.slackmacgap";
            };
            run = [
              "move-node-to-workspace 5"
            ];
          }
          {
            check-further-callbacks = false;
            "if" = {
              app-id = "com.apple.mail";
            };
            run = [
              "move-node-to-workspace 5"
            ];
          }
          {
            check-further-callbacks = false;
            "if" = {
              app-id = "com.apple.Music";
            };
            run = [
              "move-node-to-workspace 6"
            ];
          }
          {
            check-further-callbacks = false;
            "if" = {
              app-id = "md.obsidian";
            };
            run = [
              "move-node-to-workspace 7"
            ];
          }
        ];
      };
    };
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
