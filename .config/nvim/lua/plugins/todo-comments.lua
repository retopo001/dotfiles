-- todo-comments.nvim: Highlight and navigate TODO comments
return {
  "folke/todo-comments.nvim",
  event = { "BufReadPost", "BufNewFile" },
  dependencies = { "nvim-lua/plenary.nvim" },
  opts = {
    signs = false, -- Don't show signs in gutter
  },
  keys = {
    {
      "]t",
      function()
        require("todo-comments").jump_next()
      end,
      desc = "Next todo comment",
    },
    {
      "[t",
      function()
        require("todo-comments").jump_prev()
      end,
      desc = "Previous todo comment",
    },
    {
      "<leader>TA",
      "<cmd>TodoTelescope<CR>",
      desc = "TodoTelescope",
    },
    {
      "<leader>TT",
      "<cmd>TodoTelescope keywords=TODO,FIX<CR>",
      desc = "TodoTelescope (TODO/FIX only)",
    },
  },
}

