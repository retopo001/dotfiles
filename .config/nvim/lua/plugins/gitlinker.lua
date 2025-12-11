-- gitlinker.nvim: Generate GitHub/GitLab links to code
return {
  "linrongbin16/gitlinker.nvim",
  cmd = "GitLink",
  opts = {},
  keys = {
    { "<leader>gy", "<cmd>GitLink<cr>", desc = "Yank git link" },
  },
}

