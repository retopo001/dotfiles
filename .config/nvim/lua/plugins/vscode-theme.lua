return {
  "Mofiqul/vscode.nvim",
  lazy = false,
  priority = 1000,
  opts = {
    style = "dark",
    transparent = false,
    italic_comments = true,
    underline_links = true,
    disable_nvimtree_bg = true,
  },
  config = function(_, opts)
    require("vscode").setup(opts)
    vim.cmd.colorscheme "vscode"

    -- Apply cmp highlights (run now and on ColorScheme change)
    local function apply_cmp_highlights()
      -- VS Code style blue selection - very visible
      vim.api.nvim_set_hl(0, "PmenuSel", { bg = "#0e639c", fg = "#ffffff", bold = true })
      vim.api.nvim_set_hl(0, "Pmenu", { bg = "#252526", fg = "#cccccc" })
      vim.api.nvim_set_hl(0, "CmpPmenu", { bg = "#252526", fg = "#cccccc" })
      vim.api.nvim_set_hl(0, "CmpDoc", { bg = "#1e1e1e" })
      vim.api.nvim_set_hl(0, "CmpBorder", { fg = "#454545" })
      vim.api.nvim_set_hl(0, "CmpDocBorder", { fg = "#454545" })
    end

    apply_cmp_highlights()
    vim.api.nvim_create_autocmd("ColorScheme", {
      callback = apply_cmp_highlights,
    })
  end,
}
