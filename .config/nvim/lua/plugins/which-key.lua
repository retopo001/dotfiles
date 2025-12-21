-- which-key.nvim: Full panel mode for rehearsal and overview
-- Configured for large, non-scroll panel showing all mappings at once
-- IMPORTANT: Disabled in vscode-neovim because it tries to sync layout with vscode API
-- which causes "vscode.internal" errors. Use Cursor's built-in command palette instead.

return {
  "folke/which-key.nvim",
  lazy = false,  -- Load immediately, not lazy

  -- Aggressively disable in vscode-neovim to avoid layout sync errors
  cond = function()
    return not (vim.g.vscode == 1)
  end,

  init = function()
    vim.o.timeout = true
    vim.o.timeoutlen = 300
  end,

  opts = {
    -- Window configuration (v3 format)
    win = {
      border = "rounded",
      padding = { 1, 2 },  -- { top/bottom, left/right }
      wo = {
        winblend = 0,
      },
    },

    -- Don't use custom triggers - let which-key auto-detect
    -- triggers = "auto" is the default in v3

    -- Only show mappings that actually have descriptions
    filter = function(mapping)
      return mapping.desc and mapping.desc ~= ""
    end,

    plugins = {
      marks = true,
      registers = true,
      spelling = {
        enabled = true,
        suggestions = 20,
      },
    },
  },

  config = function(_, opts)
    if vim.g.vscode == 1 then
      return
    end

    local wk = require("which-key")
    wk.setup(opts)

    -- Register group names for descriptive labels
    wk.add({
      { "<leader>a", group = "AI" },
      { "<leader>b", group = "Buffer" },
      { "<leader>c", group = "Code" },
      { "<leader>d", group = "Debug" },
      { "<leader>f", group = "Find" },
      { "<leader>g", group = "Git" },
      { "<leader>h", group = "Harpoon" },
      { "<leader>m", group = "Marks" },
      { "<leader>p", group = "Project" },
      { "<leader>r", group = "Refactor" },
      { "<leader>s", group = "Search" },
      { "<leader>t", group = "Toggle" },
      { "<leader>T", group = "Treesitter" },
      { "<leader>w", group = "Window" },
      { "<leader>x", group = "Trouble" },
      { "<leader>y", group = "Yank" },
    })

    -- Register custom mappings (v3 spec)
    wk.add({
      { "<leader>ac", "<cmd>Claude<cr>", desc = "Launch Claude", mode = "n" },
      {
        "<leader>gw",
        "<cmd>edit /home/bw/dotfiles/docs/CURSOR-WORKFLOW-GUIDE.md<cr>",
        desc = "Open workflow guide",
        mode = "n",
      },
    })
  end,
}
