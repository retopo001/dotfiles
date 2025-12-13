local wezterm = require "wezterm"
local mux = wezterm.mux

-- Window size (in character cells - columns and rows)
local initial_cols = 251
local initial_rows = 76

-- Window position (in pixels)
local window_x = 1146
local window_y = 0

local config = {
  -- Default to WSL archlinux
  default_domain = "WSL:archlinux",

  -- Font settings (Nerd Fonts for icons)
  -- Point WezTerm directly to the font directory (Windows path)
  -- This allows WezTerm to find fonts even if they're not registered with Windows
  font_dirs = { wezterm.home_dir .. "\\AppData\\Local\\Microsoft\\Windows\\Fonts" },
  -- Primary font: CaskaydiaCove NF (Nerd Fonts patched version of Cascadia Code)
  font = wezterm.font("CaskaydiaCove NF"),
  font_size = 11.5,

  -- Terminal appearance
  enable_tab_bar = true,
  hide_tab_bar_if_only_one_tab = false,
  use_fancy_tab_bar = true,
  window_decorations = "RESIZE",
  color_scheme = "Catppuccin Mocha",

  -- Keybindings
  keys = {
    -- Ctrl+Shift+T → new independent bash tab (no tmux)
    {
      key = "t",
      mods = "CTRL|SHIFT",
      action = wezterm.action.SpawnCommandInNewTab {
        domain = { DomainName = "WSL:archlinux" },
        cwd = "/home/bw/signal-assembly-platform",
        args = { "bash", "-c", "export WEZTERM_NOTMUX=1; cd ~/signal-assembly-platform; exec bash -l" },
      },
    },

    -- Ctrl+Shift+N → nvim in new tab
    {
      key = "n",
      mods = "CTRL|SHIFT",
      action = wezterm.action.SpawnCommandInNewTab {
        domain = { DomainName = "WSL:archlinux" },
        cwd = "/home/bw/signal-assembly-platform",
        args = { "bash", "-c", "export WEZTERM_NOTMUX=1; cd ~/signal-assembly-platform; exec nvim" },
      },
    },
  },
}

-- Set initial window position and size on startup
wezterm.on("gui-startup", function(cmd)
  wezterm.log_info("gui-startup event fired!")
  local tab, pane, window = mux.spawn_window({
    args = cmd and cmd.args or nil,
    position = {
      x = window_x,
      y = window_y,
      origin = "MainScreen"
    },
    width = initial_cols,
    height = initial_rows,
  })
  wezterm.log_info(string.format("Spawned window at %d,%d with size %dx%d", window_x, window_y, initial_cols, initial_rows))
  
  -- Also try setting position after window is created (in case spawn_window params don't work)
  local gui_window = window:gui_window()
  if gui_window then
    wezterm.log_info("Attempting to set position via gui_window()")
    gui_window:set_position(window_x, window_y)
  end
end)

-- Handle gui-attached to set position/size for windows that already exist
wezterm.on("gui-attached", function(domain)
  wezterm.log_info("gui-attached event fired!")
  for _, window in ipairs(mux.all_windows()) do
    local gui_window = window:gui_window()
    if gui_window then
      gui_window:set_position(window_x, window_y)
      wezterm.log_info(string.format("Setting window position to %d,%d", window_x, window_y))
    end
  end
end)

-- Try setting position when window gains focus (alternative approach)
wezterm.on("window-focus-changed", function(window, pane)
  if window and window:is_focused() then
    local gui_window = window:gui_window()
    if gui_window then
      wezterm.log_info("Window focused, setting position")
      gui_window:set_position(window_x, window_y)
    end
  end
end)

return config

