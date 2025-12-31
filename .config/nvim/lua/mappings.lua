local map = vim.keymap.set

-- Better escape
map("i", "jk", "<ESC>")

-- Clear search highlights
map("n", "<Esc>", "<cmd>noh<CR>", { desc = "Clear search highlights" })

-- Window navigation (vim-tmux-navigator)
map("n", "<C-h>", "<cmd>TmuxNavigateLeft<cr>", { desc = "Navigate left (vim/tmux)" })
map("n", "<C-j>", "<cmd>TmuxNavigateDown<cr>", { desc = "Navigate down (vim/tmux)" })
map("n", "<C-k>", "<cmd>TmuxNavigateUp<cr>", { desc = "Navigate up (vim/tmux)" })
map("n", "<C-l>", "<cmd>TmuxNavigateRight<cr>", { desc = "Navigate right (vim/tmux)" })

-- Terminal mode: exit to normal mode
map("t", "<C-\\><C-n>", "<C-\\><C-n>", { desc = "Exit terminal mode" })
map("t", "<Esc>", "<C-\\><C-n>", { desc = "Exit terminal mode (alternative)" })

-- Buffer navigation
map("n", "<Tab>", "<cmd>bnext<CR>", { desc = "Next buffer" })
map("n", "<S-Tab>", "<cmd>bprev<CR>", { desc = "Previous buffer" })
map("n", "<leader>x", "<cmd>bdelete<CR>", { desc = "Close buffer" })

-- Save
map("n", "<C-s>", "<cmd>w<CR>", { desc = "Save file" })

-- Copy/paste improvements
map("v", "p", '"_dP', { desc = "Paste without yanking" })

-- Move lines
map("v", "J", ":m '>+1<CR>gv=gv", { desc = "Move line down" })
map("v", "K", ":m '<-2<CR>gv=gv", { desc = "Move line up" })

-- Keep cursor centered
map("n", "<C-d>", "<C-d>zz", { desc = "Scroll down centered" })
map("n", "<C-u>", "<C-u>zz", { desc = "Scroll up centered" })
map("n", "n", "nzzzv", { desc = "Next search centered" })
map("n", "N", "Nzzzv", { desc = "Prev search centered" })

-- Telescope (main keys defined in plugin spec, these are extras)
map("n", "<leader>fc", "<cmd>Telescope git_commits<CR>", { desc = "Git commits" })
map("n", "<leader>fs", "<cmd>Telescope git_status<CR>", { desc = "Git status" })

-- Comment toggle (using Comment.nvim)
map("n", "<leader>/", function()
  require("Comment.api").toggle.linewise.current()
end, { desc = "Toggle comment" })
map("v", "<leader>/", "<ESC><cmd>lua require('Comment.api').toggle.linewise(vim.fn.visualmode())<CR>", { desc = "Toggle comment" })

-- Claude launcher
vim.api.nvim_create_user_command("Claude", function()
  vim.cmd("terminal cld")
end, { desc = "Launch Claude in terminal buffer" })

map("n", "<leader>ac", "<cmd>Claude<cr>", { desc = "Launch Claude" })
map("n", "<leader>gw", "<cmd>edit /home/bw/dotfiles/docs/CURSOR-WORKFLOW-GUIDE.md<cr>", { desc = "Open workflow guide" })

-- Repeatable percentage jumps
local repeatable = require("utils.repeatable")
repeatable.setup_percent_repeat()

-- LSP Navigation
local nav = require("utils.navigation")

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function()
    local bufnr = vim.api.nvim_get_current_buf()

    map("n", "gd", nav.goto_definition, { buffer = bufnr, desc = "Go to definition" })
    map("n", "gr", nav.goto_references, { buffer = bufnr, desc = "Find references" })
    map("n", "gi", nav.goto_implementation, { buffer = bufnr, desc = "Go to implementation" })
    map("n", "]d", function() nav.navigate_lsp_results("next") end, { buffer = bufnr, desc = "Next definition/reference" })
    map("n", "[d", function() nav.navigate_lsp_results("prev") end, { buffer = bufnr, desc = "Prev definition/reference" })
    map("n", "]w", nav.next_word_with_definition, { buffer = bufnr, desc = "Next word with definition" })
    map("n", "[w", nav.prev_word_with_definition, { buffer = bufnr, desc = "Prev word with definition" })
  end,
})

-- File path yanking
map("n", "<leader>yf", function()
  local path = vim.api.nvim_buf_get_name(0)
  if path and path ~= "" then
    vim.fn.setreg("+", path)
    vim.fn.setreg('"', path)
    vim.notify("Yanked: " .. path, vim.log.levels.INFO)
  else
    vim.notify("No file path available", vim.log.levels.WARN)
  end
end, { desc = "Yank file absolute path" })

map("n", "<leader>yr", function()
  local path = vim.api.nvim_buf_get_name(0)
  if path and path ~= "" then
    local rel_path = vim.fn.fnamemodify(path, ":~:.")
    vim.fn.setreg("+", rel_path)
    vim.fn.setreg('"', rel_path)
    vim.notify("Yanked: " .. rel_path, vim.log.levels.INFO)
  else
    vim.notify("No file path available", vim.log.levels.WARN)
  end
end, { desc = "Yank file relative path" })
