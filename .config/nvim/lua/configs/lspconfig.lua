require("nvchad.configs.lspconfig").defaults()

-- Override rename to use inc-rename.nvim (better UX with live preview)
local map = vim.keymap.set
vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    local bufnr = args.buf
    local function opts(desc)
      return { buffer = bufnr, desc = "LSP " .. desc }
    end

    -- Replace default rename with inc-rename
    map("n", "<leader>ra", function()
      local inc_rename = require("inc_rename")
      return ":" .. inc_rename.config.cmd_name .. " " .. vim.fn.expand("<cword>")
    end, vim.tbl_extend("force", opts("Rename (inc-rename)"), { expr = true }))
  end,
})

-- LSP servers to enable (mason-lspconfig handles installation in plugins/init.lua)
local servers = {
  -- Web development
  "html",           -- HTML
  "cssls",          -- CSS
  "tailwindcss",    -- Tailwind CSS
  "ts_ls",          -- TypeScript & JavaScript (handles React, Next.js, etc.)
  "eslint",         -- ESLint for JS/TS
  
  -- Systems & compiled languages
  "rust_analyzer",  -- Rust
  "clangd",         -- C/C++
  "gopls",          -- Go
  "taplo",          -- TOML (Cargo.toml, etc.)
}

-- Enable the LSP servers
-- Note: mason-lspconfig is configured in plugins/init.lua to auto-install these servers
vim.lsp.enable(servers)

-- Enhanced ESLint configuration (better config detection, especially for monorepos)
vim.lsp.config("eslint", {
  settings = {
    -- Auto-detect eslintrc in subfolders (great for monorepos)
    workingDirectories = { mode = "auto" },
    -- Enable ESLint formatting
    format = true,
    -- Run linting as you type
    run = "onType",
    -- Validate on save
    validate = "on",
  },
})

-- Tailwind CSS: Better monorepo support (find .git root instead of package.json)
vim.lsp.config("tailwindcss", {
  root_dir = function(...)
    return require("lspconfig.util").root_pattern(".git")(...)
  end,
})

-- TypeScript: Enhanced inlay hints configuration
vim.lsp.config("ts_ls", {
  settings = {
    typescript = {
      inlayHints = {
        includeInlayParameterNameHints = "literal",
        includeInlayParameterNameHintsWhenArgumentMatchesName = false,
        includeInlayFunctionParameterTypeHints = true,
        includeInlayVariableTypeHints = false,
        includeInlayPropertyDeclarationTypeHints = true,
        includeInlayFunctionLikeReturnTypeHints = true,
        includeInlayEnumMemberValueHints = true,
      },
    },
    javascript = {
      inlayHints = {
        includeInlayParameterNameHints = "all",
        includeInlayParameterNameHintsWhenArgumentMatchesName = false,
        includeInlayFunctionParameterTypeHints = true,
        includeInlayVariableTypeHints = true,
        includeInlayPropertyDeclarationTypeHints = true,
        includeInlayFunctionLikeReturnTypeHints = true,
        includeInlayEnumMemberValueHints = true,
      },
    },
  },
})

-- Optional: Configure specific LSP servers with custom options
-- Example for TypeScript/React:
-- require("lspconfig").tsserver.setup({
--   settings = {
--     typescript = {
--       inlayHints = {
--         includeInlayParameterNameHints = "all",
--         includeInlayVariableTypeHints = true,
--       },
--     },
--   },
-- })

-- read :h vim.lsp.config for changing options of lsp servers 
