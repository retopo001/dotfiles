-- oil.nvim: Modern file explorer (replaces netrw)
return {
  "stevearc/oil.nvim",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  opts = {
    view_options = {
      show_hidden = true,
    },
  },
  keys = {
    { "-", function() require("oil").open() end, desc = "Open parent directory" },
    { "<leader>e", "<cmd>Oil<cr>", desc = "File explorer" },
  },
}

