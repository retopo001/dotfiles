local autocmd = vim.api.nvim_create_autocmd

-- Ensure syntax highlighting is enabled when files are loaded
autocmd({ "BufReadPost", "BufNewFile", "BufEnter" }, {
  callback = function()
    local ft = vim.bo.filetype
    local buftype = vim.bo.buftype
    
    -- Skip for special buffer types
    if buftype ~= "" and buftype ~= "acwrite" then
      return
    end
    
    -- Force filetype detection if empty and we have a filename
    if ft == "" and vim.api.nvim_buf_get_name(0) ~= "" then
      vim.cmd("filetype detect")
      ft = vim.bo.filetype
    end
    
    -- Enable syntax highlighting
    if vim.bo.syntax == "" then
      vim.cmd("syntax enable")
    end
    
    -- Ensure Treesitter attaches (if available)
    if ft ~= "" then
      pcall(function()
        vim.cmd("TSBufEnable highlight")
      end)
    end
  end,
})

-- Auto-convert Windows line endings (CRLF) to Unix (LF)
autocmd("BufReadPost", {
  callback = function()
    -- Check for actual ^M characters in buffer
    local lines = vim.api.nvim_buf_get_lines(0, 0, 1, false)
    if lines[1] and lines[1]:find("\r") then
      vim.cmd([[%s/\r$//e]])
      vim.bo.fileformat = "unix"
      vim.notify("Converted CRLF → LF", vim.log.levels.INFO)
    elseif vim.bo.fileformat == "dos" then
      vim.bo.fileformat = "unix"
      vim.notify("Converted CRLF → LF", vim.log.levels.INFO)
    end
  end,
})

-- Highlight on yank
autocmd("TextYankPost", {
  callback = function()
    vim.highlight.on_yank({ timeout = 200 })
  end,
})

-- Remove trailing whitespace on save
autocmd("BufWritePre", {
  pattern = "*",
  callback = function()
    local save_cursor = vim.fn.getpos(".")
    vim.cmd([[%s/\s\+$//e]])
    vim.fn.setpos(".", save_cursor)
  end,
})

-- Auto-resize splits when terminal is resized
autocmd("VimResized", {
  callback = function()
    vim.cmd("tabdo wincmd =")
  end,
})

-- Return to last edit position when opening files
autocmd("BufReadPost", {
  callback = function()
    local mark = vim.api.nvim_buf_get_mark(0, '"')
    local lcount = vim.api.nvim_buf_line_count(0)
    if mark[1] > 0 and mark[1] <= lcount then
      pcall(vim.api.nvim_win_set_cursor, 0, mark)
    end
  end,
})

-- Fix dimmed screen after closing floating windows
autocmd("WinClosed", {
  callback = function()
    vim.defer_fn(function()
      vim.cmd("mode")
    end, 10)
  end,
})

-- Close some filetypes with q
autocmd("FileType", {
  pattern = { "help", "qf", "lspinfo", "man", "notify", "checkhealth" },
  callback = function(event)
    vim.bo[event.buf].buflisted = false
    vim.keymap.set("n", "q", "<cmd>close<cr>", { buffer = event.buf, silent = true })
  end,
})

-- Enable word wrap in Telescope preview window
autocmd("User", {
  pattern = "TelescopePreviewerLoaded",
  callback = function(args)
    -- Use a deferred function to ensure window exists
    vim.defer_fn(function()
      if args.buf and vim.api.nvim_buf_is_valid(args.buf) then
        local win = vim.fn.bufwinid(args.buf)
        if win ~= -1 and vim.api.nvim_win_is_valid(win) then
          pcall(function()
            vim.wo[win].wrap = true
          end)
        end
      end
    end, 10)
  end,
})
