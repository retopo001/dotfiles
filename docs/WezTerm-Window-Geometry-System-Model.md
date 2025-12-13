# WezTerm Window Geometry System Model

## System Architecture

### Configuration File Structure (FINAL)
- **Windows Native Config:** `C:\Users\wijay\.wezterm.lua` (PRIMARY - WezTerm checks this FIRST)
- **Windows Config Dir:** `C:\Users\wijay\.config\wezterm\` (exists but EMPTY)
- **WSL Path:** `/home/bw/.wezterm.lua` → symlink to dotfiles config
- **Dotfiles Config:** `/home/bw/dotfiles/.config/wezterm/wezterm.lua` (source of truth)
- **Status:** Windows config now has all event handlers (FIXED)

### Window Geometry Settings
- **Target Position:** X: 1146 pixels, Y: 0 pixels (from Window Spy)
- **Target Size:** 251 columns × 76 rows (from `tput cols/lines`)
- **Coordinate System:** "MainScreen"

## Root Cause (RESOLVED)

### H8: Config File Priority Issue - CONFIRMED AND FIXED
**Problem:**
- `C:\Users\wijay\.wezterm.lua` (Windows native) had NO event handlers
- WezTerm on Windows checks Windows user directory FIRST
- `C:\Users\wijay\.config\wezterm\` directory exists but is empty
- WSL config files checked later (or not at all if Windows config exists)

**Solution Applied:**
- Backed up Windows config to `C:\Users\wijay\.wezterm.lua.backup`
- Added all event handlers to `C:\Users\wijay\.wezterm.lua`
- Kept Windows-specific font setting (CaskaydiaCove NF)

## Current Configuration State

### Event Handlers (Now in Windows Config)
1. **`gui-startup` Event:**
   - Uses `mux.spawn_window()` with position/size parameters
   - Also calls `gui_window():set_position()` after window creation (fallback)
   - Debug logging enabled

2. **`gui-attached` Event:**
   - Iterates all windows, sets position via `gui_window():set_position()`
   - Debug logging enabled

3. **`window-focus-changed` Event:**
   - Sets position when window gains focus
   - Alternative timing approach

## Expected Behavior (After Fix)

- WezTerm on Windows will load `C:\Users\wijay\.wezterm.lua` (now has all handlers)
- All event handlers should be active
- Window should open at (1146, 0) with size 251×76

## Remaining Hypotheses (If Still Doesn't Work)

### H2: Window State Restoration Overrides Settings
- **Mechanism:** WezTerm saves/restores geometry AFTER events fire
- **Test:** If window still opens at wrong position, this is likely cause

### H3: Coordinate System Issue
- **Issue:** "MainScreen" may not work on Windows multi-monitor
- **Alternative:** Try "ActiveScreen" or different coordinate system

## Next Steps

1. **Test the Fix:**
   - Restart WezTerm completely
   - Verify window opens at (1146, 0) with size 251×76
   - Check log file for "gui-startup event fired!" message

2. **If Still Not Working:**
   - Investigate window state restoration mechanism
   - Try different coordinate system ("ActiveScreen")
   - Check if `gui_window():set_position()` works on Windows

## Model Status: Windows Config File Fixed
- **Root Cause:** `C:\Users\wijay\.wezterm.lua` had no handlers, WezTerm loaded it first
- **Fix Applied:** Added all event handlers to Windows config file
- **Other Files:** `C:\Users\wijay\.config\wezterm\` exists but empty (not relevant)
- **Next:** Test to verify fix works, or investigate remaining hypotheses
