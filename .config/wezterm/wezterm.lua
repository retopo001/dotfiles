local wezterm = require "wezterm"
local mux = wezterm.mux
local act = wezterm.action


-- Window layouts
local layouts = {
  main = { x = 1146, y = 31, width = 2301, height = 1416 },
  full = { x = 0, y = 0, width = 3440, height = 1440 },
  blank = { x = 1146, y = 31, width = 2301, height = 1416 },
}

-- Default window position and size (main layout)
local window_x = layouts.main.x
local window_y = layouts.main.y
local window_width = layouts.main.width
local window_height = layouts.main.height

-- Helper: spawn new window with layout
-- Uses custom script that auto-increments session names (main-1, main-2, etc.)
local function spawn_layout(layout_name)
  return wezterm.action_callback(function(window, pane)
    -- Use PowerShell Start-Process to spawn detached
    os.execute('powershell.exe -NoProfile -Command "Start-Process wezterm.exe -ArgumentList \'start\',\'--cwd\',\'/home/bw\',\'--\',\'wsl.exe\',\'-d\',\'archlinux\',\'-e\',\'zsh\',\'-lc\',\'/home/bw/bin/layout ' .. layout_name .. '\'"')
  end)
end

local config = wezterm.config_builder and wezterm.config_builder() or {}

-- Default to WSL archlinux
config.default_domain = "WSL:archlinux"

-- Font settings
config.font_dirs = { wezterm.home_dir .. "\\AppData\\Local\\Microsoft\\Windows\\Fonts" }
config.font = wezterm.font_with_fallback({
  { family = "JetBrainsMono Nerd Font", weight = "Regular" },
  { family = "JetBrains Mono", weight = "Regular" },
  "Consolas",
})
config.font_size = 10

-- Terminal appearance
config.enable_tab_bar = false
config.hide_tab_bar_if_only_one_tab = true
config.use_fancy_tab_bar = false
config.window_decorations = "TITLE | RESIZE"
config.color_scheme = "Vs Code Dark+ (Gogh)"
config.window_close_confirmation = "NeverPrompt"

-- Keybindings
config.keys = {
  -- Ctrl+Shift+T → new independent zsh tab (no tmux)
  {
    key = "t",
    mods = "CTRL|SHIFT",
    action = wezterm.action.SpawnCommandInNewTab {
      domain = { DomainName = "WSL:archlinux" },
      cwd = "/home/bw",
      args = { "zsh", "-c", "export WEZTERM_NOTMUX=1; exec zsh -l" },
    },
  },

  -- Ctrl+Shift+N → nvim in new tab
  {
    key = "n",
    mods = "CTRL|SHIFT",
    action = wezterm.action.SpawnCommandInNewTab {
      domain = { DomainName = "WSL:archlinux" },
      cwd = "/home/bw",
      args = { "zsh", "-c", "export WEZTERM_NOTMUX=1; exec nvim" },
    },
  },

  -- Alt+Shift+V → side by side (vertical divider |) - opens in current directory
  {
    key = "V",
    mods = "ALT|SHIFT",
    action = wezterm.action_callback(function(window, pane)
      local cwd = pane:get_current_working_dir()
      local cwd_path = cwd and cwd.file_path or "/home/bw"
      window:perform_action(wezterm.action.SplitHorizontal {
        domain = { DomainName = "WSL:archlinux" },
        cwd = cwd_path,
        args = { "zsh", "-c", "export WEZTERM_NOTMUX=1; exec zsh -l" },
      }, pane)
    end),
  },

  -- Alt+Shift+H → stacked (horizontal divider ─) - opens in current directory
  {
    key = "H",
    mods = "ALT|SHIFT",
    action = wezterm.action_callback(function(window, pane)
      local cwd = pane:get_current_working_dir()
      local cwd_path = cwd and cwd.file_path or "/home/bw"
      window:perform_action(wezterm.action.SplitVertical {
        domain = { DomainName = "WSL:archlinux" },
        cwd = cwd_path,
        args = { "zsh", "-c", "export WEZTERM_NOTMUX=1; exec zsh -l" },
      }, pane)
    end),
  },

  -- Smart paste: if clipboard has image, save to Dropbox and paste path; otherwise normal paste
  {
    key = "V",
    mods = "CTRL|SHIFT",
    action = wezterm.action_callback(function(window, pane)
      -- PowerShell: check for image, save it, output WSL path
      local success, stdout, stderr = wezterm.run_child_process({
        "powershell.exe", "-NoProfile", "-Command", [[
          $img = Get-Clipboard -Format Image
          if ($img) {
            $timestamp = Get-Date -Format "yyyyMMdd_HHmmss"
            $winpath = "$env:USERPROFILE\Dropbox\Downloads\clipboard_$timestamp.png"
            $img.Save($winpath)
            # Convert to WSL path
            $drive = $winpath.Substring(0,1).ToLower()
            $rest = $winpath.Substring(2).Replace('\','/')
            Write-Output "/mnt/$drive$rest"
          }
        ]]
      })
      local path = stdout and stdout:gsub("%s+$", "") or ""
      if success and path:match("^/mnt/") then
        -- Got a valid path, paste it
        pane:send_text(path)
      else
        -- No image or error, do normal paste
        window:perform_action(act.PasteFrom("Clipboard"), pane)
      end
    end),
  },

  -- Alt+hjkl → WezTerm panes | Ctrl+hjkl → tmux panes (passes through)
  { key = "h", mods = "ALT", action = act.ActivatePaneDirection "Left" },
  { key = "j", mods = "ALT", action = act.ActivatePaneDirection "Down" },
  { key = "k", mods = "ALT", action = act.ActivatePaneDirection "Up" },
  { key = "l", mods = "ALT", action = act.ActivatePaneDirection "Right" },

  -- Copy mode (vim-like scrollback navigation)
  -- Ctrl+Shift+X → enter copy mode, then use hjkl, v, V, y, /, ?, n, N
  { key = "X", mods = "CTRL|SHIFT", action = act.ActivateCopyMode },

  -- Quick scroll without entering copy mode
  { key = "PageUp", mods = "SHIFT", action = act.ScrollByPage(-1) },
  { key = "PageDown", mods = "SHIFT", action = act.ScrollByPage(1) },
  { key = "UpArrow", mods = "SHIFT", action = act.ScrollByLine(-1) },
  { key = "DownArrow", mods = "SHIFT", action = act.ScrollByLine(1) },

  -- Command palette (shows all available actions)
  { key = "P", mods = "CTRL|SHIFT", action = act.ActivateCommandPalette },

  -- Layout launchers (new window + tmuxifier session)
  { key = "1", mods = "ALT|SHIFT", action = spawn_layout("main") },
  { key = "2", mods = "ALT|SHIFT", action = spawn_layout("full") },
  { key = "3", mods = "ALT|SHIFT", action = spawn_layout("blank") },
}

-- Set initial window position and size on startup (in pixels)
wezterm.on("gui-startup", function(cmd)
  local tab, pane, window = mux.spawn_window(cmd or {})
  local gui = window:gui_window()
  gui:set_inner_size(window_width, window_height)
  gui:set_position(window_x, window_y)
end)

return config
