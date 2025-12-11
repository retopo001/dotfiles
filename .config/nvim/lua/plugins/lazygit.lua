-- lazygit.nvim: Floating terminal wrapper for lazygit
-- Better than git.nvim for full git workflow
return {
  "kdheepak/lazygit.nvim",
  cmd = {
    "LazyGit",
    "LazyGitConfig",
    "LazyGitCurrentFile",
    "LazyGitFilter",
    "LazyGitFilterCurrentFile",
  },
  keys = {
    { "<leader>gg", "<cmd>LazyGit<cr>", desc = "LazyGit" },
  },
  config = function()
    -- lazygit.nvim automatically sets up floating terminal
    -- No additional config needed
  end,
}

