require "nvchad.autocmds"

-- Fix dimmed screen after closing floating windows (Mason, etc.)
vim.api.nvim_create_autocmd("WinClosed", {
  callback = function()
    vim.defer_fn(function()
      vim.cmd("mode")
    end, 10)
  end,
})
