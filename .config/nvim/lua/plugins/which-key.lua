-- which-key.nvim: Shows available keybindings
-- NOTE: <leader>g/d/w/b/f are handled by Hydra.nvim for sticky submodes
-- This file handles everything else

return {
  "folke/which-key.nvim",
  lazy = false,

  cond = function()
    return not (vim.g.vscode == 1)
  end,

  init = function()
    vim.o.timeout = true
    vim.o.timeoutlen = 300
  end,

  opts = {
    win = {
      border = "rounded",
      padding = { 1, 2 },
      wo = { winblend = 0 },
    },
    filter = function(mapping)
      return mapping.desc and mapping.desc ~= ""
    end,
    plugins = {
      marks = true,
      registers = true,
      spelling = { enabled = true, suggestions = 20 },
    },
    keys = {
      scroll_down = "<C-d>",
      scroll_up = "<C-u>",
    },
    show_help = true,
    show_keys = true,
  },

  config = function(_, opts)
    if vim.g.vscode == 1 then
      return
    end

    local wk = require("which-key")
    wk.setup(opts)

    -- ============================================
    -- GROUP LABELS (non-Hydra groups only)
    -- ============================================
    wk.add({
      { "<leader>a", group = "AI" },
      { "<leader>c", group = "Code" },
      { "<leader>h", group = "Harpoon" },
      { "<leader>l", group = "LSP" },
      { "<leader>q", group = "Quit" },
      { "<leader>t", group = "Toggle" },
      { "<leader>x", group = "Trouble" },
      { "<leader>y", group = "Yank" },
      { "<leader>r", group = "Refactor" },
      { "<leader>s", group = "Search" },
      { "<leader>m", group = "Marks" },
      { "<leader>p", group = "Project" },
      { "<leader>T", group = "Treesitter" },
      { "gr", group = "LSP refs/rename" },
    })

    -- ============================================
    -- QUICK ACCESS - Single keys on leader
    -- ============================================
    wk.add({
      { "<leader>/", function() require("Comment.api").toggle.linewise.current() end, desc = "Comment line" },
      { "<leader>e", "<cmd>Oil<cr>", desc = "File explorer" },
      { "<leader>u", "<cmd>UndotreeToggle<cr>", desc = "Undo tree" },
      { "<leader>?", "<cmd>Telescope keymaps<cr>", desc = "ALL keymaps (search)" },
      { "<leader>[", function() wk.show({ keys = "[", mode = "n" }) end, desc = "[ Prev motions" },
      { "<leader>]", function() wk.show({ keys = "]", mode = "n" }) end, desc = "] Next motions" },
    })

    -- ============================================
    -- MAPPINGS (non-Hydra groups)
    -- ============================================
    wk.add({
      -- AI
      { "<leader>ac", "<cmd>Claude<cr>", desc = "Claude" },

      -- Code
      { "<leader>ca", vim.lsp.buf.code_action, desc = "Action" },
      { "<leader>cf", function() require("conform").format() end, desc = "Format" },
      { "<leader>cr", vim.lsp.buf.rename, desc = "Rename" },

      -- Harpoon
      { "<leader>ha", function() require("harpoon"):list():add() end, desc = "Add" },
      { "<leader>hh", function() require("harpoon").ui:toggle_quick_menu(require("harpoon"):list()) end, desc = "Menu" },
      { "<leader>h1", function() require("harpoon"):list():select(1) end, desc = "1" },
      { "<leader>h2", function() require("harpoon"):list():select(2) end, desc = "2" },
      { "<leader>h3", function() require("harpoon"):list():select(3) end, desc = "3" },
      { "<leader>h4", function() require("harpoon"):list():select(4) end, desc = "4" },

      -- LSP
      { "<leader>li", "<cmd>LspInfo<cr>", desc = "Info" },
      { "<leader>lr", "<cmd>LspRestart<cr>", desc = "Restart" },

      -- Quit
      { "<leader>qq", "<cmd>qa<cr>", desc = "All" },
      { "<leader>qw", "<cmd>wqa<cr>", desc = "Write+quit" },

      -- Toggle
      { "<leader>tn", "<cmd>set number!<cr>", desc = "Numbers" },
      { "<leader>tr", "<cmd>set relativenumber!<cr>", desc = "Relative" },
      { "<leader>tw", "<cmd>set wrap!<cr>", desc = "Wrap" },
      { "<leader>ts", "<cmd>set spell!<cr>", desc = "Spell" },
      { "<leader>th", function() require("close_buffers").delete({ type = "hidden" }) end, desc = "Close hidden" },

      -- Trouble
      { "<leader>xx", "<cmd>Trouble diagnostics toggle<cr>", desc = "Diagnostics" },
      { "<leader>xd", "<cmd>Trouble diagnostics toggle filter.buf=0<cr>", desc = "Buffer" },
      { "<leader>xl", "<cmd>Trouble loclist toggle<cr>", desc = "Loclist" },
      { "<leader>xq", "<cmd>Trouble qflist toggle<cr>", desc = "Quickfix" },
      { "<leader>xt", "<cmd>Trouble todo toggle<cr>", desc = "TODOs" },

      -- Yank
      { "<leader>yf", function()
          local path = vim.api.nvim_buf_get_name(0)
          vim.fn.setreg("+", path)
          vim.notify("Yanked: " .. path)
        end, desc = "Full path" },
      { "<leader>yr", function()
          local path = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(0), ":~:.")
          vim.fn.setreg("+", path)
          vim.notify("Yanked: " .. path)
        end, desc = "Relative" },
    })

    -- ============================================
    -- NON-LEADER PREFIXES (vim motions)
    -- ============================================
    wk.add({
      { "[", group = "Previous" },
      { "]", group = "Next" },
      { "g", group = "Go/LSP" },
      { "z", group = "Folds" },
      { "[d", desc = "Diagnostic" },
      { "[b", desc = "Buffer" },
      { "[q", desc = "Quickfix" },
      { "[t", desc = "Todo" },
      { "]d", desc = "Diagnostic" },
      { "]b", desc = "Buffer" },
      { "]q", desc = "Quickfix" },
      { "]t", desc = "Todo" },
      { "gd", desc = "Definition" },
      { "gr", desc = "References" },
      { "gi", desc = "Implementation" },
      { "gc", desc = "Comment" },
      { "gcc", desc = "Comment line" },
      { "zR", desc = "Open all" },
      { "zM", desc = "Close all" },
      { "za", desc = "Toggle" },
      { "s", desc = "Flash jump" },
      { "S", desc = "Flash treesitter" },
      { "K", desc = "Hover docs" },
    })
  end,
}
