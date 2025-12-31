local o = vim.opt

-- Line numbers
o.number = true
o.relativenumber = true

-- Indentation
o.expandtab = true
o.shiftwidth = 2
o.smartindent = true
o.tabstop = 2
o.softtabstop = 2

-- UI
o.cursorline = true
o.signcolumn = "yes"
o.splitbelow = true
o.splitright = true
o.termguicolors = true
o.showmode = false
o.laststatus = 3  -- Global statusline
o.scrolloff = 8

-- Search
o.ignorecase = true
o.smartcase = true
o.hlsearch = true

-- Files
o.swapfile = false
o.undofile = true
o.updatetime = 250
o.timeoutlen = 400

-- Completion
o.completeopt = "menu,menuone,noselect"

-- Command line completion (fallback when cmp-cmdline not loaded)
o.wildmode = "longest:full,full"
o.wildmenu = true

-- Clipboard (use system clipboard)
o.clipboard = "unnamedplus"

-- Mouse
o.mouse = "a"

-- Fill chars
o.fillchars = { eob = " " }
