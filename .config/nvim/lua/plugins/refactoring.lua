-- refactoring.nvim: Code refactoring tools (ThePrimeagen)
return {
  "ThePrimeagen/refactoring.nvim",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-treesitter/nvim-treesitter",
  },
  opts = {},
  keys = {
    {
      "<leader>re",
      "<cmd>lua require('refactoring').select_refactor()<CR>",
      desc = "Refactor",
      mode = { "n", "v" },
    },
  },
}

