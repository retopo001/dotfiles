-- LSP Configuration (no NvChad dependency)
local map = vim.keymap.set

-- Diagnostic configuration
vim.diagnostic.config({
  virtual_text = { prefix = "●" },
  signs = true,
  underline = true,
  update_in_insert = false,
  severity_sort = true,
  float = {
    border = "rounded",
    source = "always",
  },
})

-- Diagnostic signs
local signs = { Error = " ", Warn = " ", Hint = "󰌵 ", Info = " " }
for type, icon in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end

-- LSP keybindings on attach
-- Note: gd, gr, gi are set in mappings.lua with custom navigation wrappers
vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    local bufnr = args.buf
    local function opts(desc)
      return { buffer = bufnr, desc = "LSP " .. desc }
    end

    -- Hover and type info
    map("n", "K", function() vim.lsp.buf.hover() end, opts("Hover documentation"))
    map("n", "<leader>D", function() vim.lsp.buf.type_definition() end, opts("Go to type definition"))

    -- Actions
    map("n", "<leader>ca", function() vim.lsp.buf.code_action() end, opts("Code action"))

    -- Rename (inc-rename for better UX)
    map("n", "<leader>ra", function()
      local ok, inc_rename = pcall(require, "inc_rename")
      if ok then
        return ":" .. inc_rename.config.cmd_name .. " " .. vim.fn.expand("<cword>")
      else
        vim.lsp.buf.rename()
      end
    end, vim.tbl_extend("force", opts("Rename"), { expr = true }))

    -- Diagnostics
    map("n", "[e", function() vim.diagnostic.goto_prev() end, opts("Previous diagnostic"))
    map("n", "]e", function() vim.diagnostic.goto_next() end, opts("Next diagnostic"))
    map("n", "<leader>e", function() vim.diagnostic.open_float() end, opts("Line diagnostics"))
  end,
})

-- LSP capabilities (for nvim-cmp integration)
local capabilities = vim.lsp.protocol.make_client_capabilities()
local ok, cmp_lsp = pcall(require, "cmp_nvim_lsp")
if ok then
  capabilities = vim.tbl_deep_extend("force", capabilities, cmp_lsp.default_capabilities())
end

-- Default config for all LSP servers
vim.lsp.config("*", {
  capabilities = capabilities,
})

-- LSP servers to enable
local servers = {
  -- Web development
  "html",
  "cssls",
  "tailwindcss",
  "ts_ls",
  "eslint",

  -- Systems & compiled languages
  "rust_analyzer",
  "clangd",
  "gopls",
  "taplo",

  -- Scripting & config
  "lua_ls",
}

-- Enable LSP servers
vim.lsp.enable(servers)

-- Server-specific configurations
vim.lsp.config("eslint", {
  settings = {
    workingDirectories = { mode = "auto" },
    format = true,
    run = "onType",
    validate = "on",
  },
})

vim.lsp.config("tailwindcss", {
  root_dir = function(...)
    return require("lspconfig.util").root_pattern(".git")(...)
  end,
})

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

vim.lsp.config("lua_ls", {
  settings = {
    Lua = {
      diagnostics = {
        globals = { "vim", "wezterm" },
      },
      workspace = {
        library = vim.api.nvim_get_runtime_file("", true),
        checkThirdParty = false,
      },
      telemetry = { enable = false },
    },
  },
})
