# TODO — dotfiles / nix-flake review

Findings from a review of the flake, system/, home/, hosts/ and configs/ modules.
Verified by evaluating the flake and inspecting generated artifacts where noted.

Suggested order: bugs 1–6 (all one-liners), then 7–11, then the
`helix/neovim/languages` collapse for the largest LOC reduction.

---

## 🔴 Bugs (fix first)

- [x] **1. `FZF_DEFAULTCOMMAND` typo** — `configs/fzf/default.nix:24`.
      Missing underscore, so fzf never uses `fd`. → `FZF_DEFAULT_COMMAND`.
- [x] **2. fzf vars not exported** — `configs/fzf/default.nix:24-26`,
      `configs/zsh/default.nix:118`. `FZF_DEFAULT_COMMAND` / `FZF_DEFAULT_OPTS`
      are read by the fzf *binary* (a child process); plain shell assignments are
      invisible to it. Add `export`.
- [x] **3. Broken grep regex** — `home/linux-desktop.nix:34`.
      `grep -v '^\s+$'` — in POSIX BRE `+` is a *literal* character, so blank
      lines are never stripped and `paste -sd';'` emits `cmd;;cmd`.
      → `grep -v '^[[:space:]]*$'`.
- [x] **4. Waybar has no stylesheet** — `configs/waybar/`.
      Verified `programs.waybar.style = null`, and the generated files are
      `common.css`, `style-dark.css`, `style-light.css`,
      `blueberry_peach_{dark,light}.css` — but waybar only auto-loads
      `style.css`. All 5 CSS files are dead and waybar runs with its built-in
      default styling. Nothing symlinks it either (the theme scripts don't touch
      waybar). Set `programs.waybar.style`, or write a `style.css` that
      `@import`s the others.
      -> wrong
- [x] **5. `extenstions` typo** — `configs/neovim/plugins/default.nix:303`.
      lualine silently ignores it. → `extensions`.
- [x] **6. Broken nested quoting in tmux** — `configs/tmux/default.nix:120`.
      `if-shell "[ "$(~/.config/current_theme)" = "dark" ]"` — unescaped nested
      double quotes.

---

## 🟠 High

- [ ] **7. Network access on every activation** — `configs/helix/default.nix:20-33`.
      `home.activation.hxGrammars` runs `hx --grammar fetch` + `build` on *every*
      activation. Non-declarative, needs network, and swallows all errors with
      `|| true`. The `makeBinPath` PATH export is dead code — both calls already
      use absolute `lib.getExe` paths. Nixpkgs `helix` already ships grammars.
- [ ] **8. Three nixpkgs instances in the lock** — `flake.nix:20,26`.
      `nixvim` → `nixpkgs_2`, `private-fonts` → **`nixpkgs_3` (nixos-unstable)**.
      Add `inputs.nixpkgs.follows = "nixpkgs"` to both — at minimum
      `private-fonts` (unstable glibc against a stable system).
- [ ] **9. Wrong Home Manager module for Vivaldi** — `configs/chromium/default.nix`.
      `programs.chromium.package = pkgs.vivaldi` makes HM manage
      `~/.config/chromium/*` while Vivaldi reads `~/.config/vivaldi/*`.
      HM has a real `programs.vivaldi` module (confirmed present). Switch to it.
- [ ] **10. `nix` in `home.packages`** — `home/default.nix:39`.
      Verified it lands in both the user *and* the system profile. A
      profile-level nix shadowing the daemon's is a classic breakage source.
      Remove here and in `system/common.nix:15`.
- [ ] **11. System daemons in `home.packages`** — `home/nixos-desktop.nix:14,25`.
      `networkmanager` and `pulseaudio` are system services; a user-profile
      `pulseaudio` shadows `pactl` from pipewire-pulse. Keep only `pavucontrol`
      and `networkmanagerapplet`.

---

## 🟡 Medium

- [ ] **12. Git signing declared twice** — `configs/git/default.nix:46-48,85-87,90`.
      `settings.gpg.format` + `signing.format`, and `settings.user.signingkey`
      instead of `signing.key`. Use the `signing` submodule only.
- [ ] **13. Git booleans as strings** — `configs/git/default.nix`.
      Generated config literally reads `gpgSign = "true"`. Use real booleans.
- [ ] **14. Tilde in signing key** — `configs/git/default.nix:86`.
      Git does not tilde-expand `user.signingkey` for ssh signing. Use an
      absolute path or `config.home.homeDirectory`.
- [ ] **15. ~57 lines of redundant sway keybindings** — `configs/sway/config.nix:122-179`.
      `lib.mkOptionDefault` already provides these (modifier *is* `Mod4`,
      terminal *is* `ghostty`). Keep only the genuine additions
      (`Mod4+Shift+b`, `Mod4+Control+*`, `Mod4+Shift+a`).
- [ ] **16. `config.nix` is not a module** — `configs/sway/default.nix:47-49`.
      `import ./config.nix { inherit lib pkgs; }` is a hand-applied function: no
      access to `config`, no `imports`. Make it a real module.
- [ ] **17. XDG vars duplicated** — `configs/sway/default.nix:8-23`.
      Same block verbatim in `home.sessionVariables` and
      `systemd.user.sessionVariables`; HM sets these already.
- [ ] **18. Hand-rolled mako unit** — `home/nixos-desktop.nix:44-63`.
      Duplicates HM's generated unit and hardcodes `${pkgs.mako}/bin/mako`, so
      `services.mako.package` is ignored. Override only the target.
- [ ] **19. Raw mako config file** — `configs/mako/default.nix`.
      Uses `xdg.configFile."mako/config".text` instead of `services.mako.settings`.
      Works *only* because `settings` is empty — the `text` type is `lines`, so
      setting both would silently concatenate.
- [ ] **20. Font packages duplicated** — `system/common.nix:32-40` vs
      `home/linux-desktop.nix:96-101` (same 6 packages). Pick one layer.
- [ ] **21. Duplicate packages** — verified in `home.packages`: **`bottom`**
      (`programs.bottom.enable` *and* the explicit package) and
      `adwaita-icon-theme`.
- [ ] **22. Dead `isDarwin` specialArg** — `flake.nix:69`.
      No system module reads it (`system/common.nix` computes it for HM via
      `extraSpecialArgs`), and klabautermann omits it → inconsistent.
- [ ] **23. Unused input bindings** — `flake.nix:29-40`.
      Destructures 5 inputs it never uses. → `inputs@{ self, nixpkgs, nix-darwin,
      home-manager, disko, ... }`.
- [ ] **24. Host boilerplate** — `flake.nix`. Three near-identical host blocks;
      a `mkHost` / `mkHome` helper would cut this substantially.
- [ ] **25. `system.configurationRevision`** set only for darwin — set it for the
      NixOS hosts too.
- [ ] **26. Hardcoded username** — `hosts/aegir/configuration.nix:56` uses
      `users.users.schemar` while klabautermann uses `${username}`.
- [ ] **27. Duplicate kanshi profile** — `hosts/nb0407/default.nix:29-38`.
      Two byte-identical `profile` blocks; the second is unreachable. Also raw
      text where `services.kanshi` offers typed `profiles`/`outputs`.

---

## 🔵 Biggest simplification wins

- [ ] **28. `configs/helix/languages/*` — 14 files, one shape.**
      All are `extraPackages` + `{ name; auto-format = true; }`. One attrset +
      `lib.mapAttrsToList` removes ~13 files / ~200 lines.
- [ ] **29. `configs/neovim/languages/*` — 12 files, same story** (~160–190 lines
      savable). `prettier`'s formatter command is repeated 4×.
- [ ] **30. `configs/tmux/default.nix:27-111`** — 5 near-identical `mkTmuxPlugin`
      blocks → one local helper, ~50 lines saved.
- [ ] **31. `"${inputs.blueberry-peach}/ports/..."` repeated across 12 files** →
      a `bp = p: "${inputs.blueberry-peach}/ports/${p}";` helper removes the
      noise and a whole class of typo.
- [ ] **32. Seven single-option files** — `configs/{fd,ripgrep,jq,bottom,tealdeer,avizo,theme}`
      only flip `enable = true`. Merge into one module.

---

## ⚡ Quick wins

- [ ] `system/common.nix:11` — `experimental-features` should be a **list**, not
      the string `"nix-command flakes"`.
- [ ] `system/common.nix:16` — `coreutils-prefixed` is Darwin-oriented; guard it
      with `lib.optionals pkgs.stdenv.hostPlatform.isDarwin`.
- [ ] `system/common.nix:22,27` — use `pkgs.zsh` instead of
      `"/run/current-system/sw/bin/zsh"` strings.
- [ ] `system/common.nix:29` — `home = if isDarwin …` is redundant; both NixOS
      and nix-darwin already default it correctly.
- [ ] `configs/gh/default.nix:7-10` — `with pkgs; [ ]` on an empty list.
- [ ] `configs/ssh/default.nix:1` — unused `config` argument.
- [ ] `configs/sway/config.nix:24` — `terminal = terminal;` → `inherit terminal;`.
- [ ] `configs/zsh/default.nix:98` — `export PATH=…` → `home.sessionPath`.
- [ ] `configs/zsh/default.nix:12` — `sha256 =` → `hash =`; and `zsh-helix-mode`
      belongs in `programs.zsh.plugins`.
- [ ] `configs/fzf/default.nix:28` — comment says "written in
      configs/fzf/default.nix" *inside that very file*; it's actually in
      `configs/zsh/default.nix`.
- [ ] `configs/tmux/default.nix:7` — `terminal` should be `tmux-256color`, not
      `xterm-256color`.
- [ ] `configs/tmux/default.nix:6` — `shell = "${pkgs.zsh}/bin/zsh"` pins a store
      path different from the configured login shell.
- [ ] `configs/helix/default.nix:40` and the 4 scripts in
      `home/linux-desktop.nix` — `writeShellScriptBin` / `home.file` with
      hand-written shebangs and ambient `fd`/`rg`/`fzf`/`swaymsg`. Use
      `pkgs.writeShellApplication { runtimeInputs = …; }` for hermetic,
      shellcheck-clean scripts. (The helix one also has a *duplicate* shebang
      inside the body.)
- [ ] `home/linux-desktop.nix:190` — `mkForce` on ghostty `font-size` in a shared
      module means no host can override it.
- [ ] `configs/sway/config.nix:67` — startup always runs `darkmode.sh`, so light
      mode never survives a re-login; read `~/.config/current_theme` instead.
- [ ] `configs/sway/config.nix:63` — `systemctl --user start mako` is redundant
      with `WantedBy = [ "sway-session.target" ]`.
- [ ] Inconsistent binary references across sway/waybar: some
      `${pkgs.x}/bin/x`, most bare names. Standardise on `lib.getExe`.
- [ ] `configs/firefox/default.nix` — `configPath` override with no profiles
      defined; Firefox on Linux reads `~/.mozilla/firefox` regardless. Likely
      dead config.
- [ ] `hosts/nb0407/default.nix` — inconsistent disabling (`null` vs
      `pkgs.emptyDirectory`); extract the block into `home/ubuntu-managed.nix`.
- [ ] **Add `formatter.<system> = pkgs.nixfmt;`** — nixfmt runs clean across all
      90 files today, so `nix fmt` is free. There is also no `checks` output / CI.
