require "nvchad.mappings"

-- add yours here

local map = vim.keymap.set

map("i", "jk", "<ESC>")

-- Override NvChad's window navigation to use vim-tmux-navigator
-- This allows seamless navigation between Neovim splits and tmux panes
map("n", "<C-h>", "<cmd>TmuxNavigateLeft<cr>", { desc = "Navigate left (vim/tmux)" })
map("n", "<C-j>", "<cmd>TmuxNavigateDown<cr>", { desc = "Navigate down (vim/tmux)" })
map("n", "<C-k>", "<cmd>TmuxNavigateUp<cr>", { desc = "Navigate up (vim/tmux)" })
map("n", "<C-l>", "<cmd>TmuxNavigateRight<cr>", { desc = "Navigate right (vim/tmux)" })

-- Claude launcher inside Neovim
vim.api.nvim_create_user_command("Claude", function()
  vim.cmd("terminal cld")
end, { desc = "Launch Claude in terminal buffer" })

-- Workflow guide quick access (absolute path, no cwd dependence)
map("n", "<leader>gw", "<cmd>edit /home/bw/dotfiles/WORKFLOW-GUIDE.md<cr>", { desc = "Open workflow guide" })

-- Claude launcher mapping
map("n", "<leader>ac", "<cmd>Claude<cr>", { desc = "Launch Claude" })

-- map({ "n", "i", "v" }, "<C-s>", "<cmd> w <cr>")
