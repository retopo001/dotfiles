return {
  "nvim-lualine/lualine.nvim",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  event = "VeryLazy",
  opts = function()
    -- Hydra hints component
    local function hydra_hints()
      local ok, hydra_statusline = pcall(require, "hydra.statusline")
      if ok and hydra_statusline.is_active() then
        -- Show active Hydra name
        return hydra_statusline.get_name() .. " MODE"
      else
        -- Show available Hydra prefixes
        return "[g]it [d]iag [w]in [b]uf [f]ind"
      end
    end

    return {
      options = {
        theme = "vscode",
        globalstatus = true,
        component_separators = { left = "", right = "" },
        section_separators = { left = "", right = "" },
      },
      sections = {
        lualine_a = { "mode" },
        lualine_b = { "branch", "diff", "diagnostics" },
        lualine_c = { { "filename", path = 1 } },
        lualine_x = {
          {
            hydra_hints,
            color = function()
              local ok, hydra_statusline = pcall(require, "hydra.statusline")
              if ok and hydra_statusline.is_active() then
                return { fg = "#ff9e64", gui = "bold" }  -- Orange when active
              end
              return { fg = "#7aa2f7" }  -- Blue when showing hints
            end,
          },
          "filetype",
        },
        lualine_y = { "progress" },
        lualine_z = { "location" },
      },
      extensions = { "lazy", "mason", "oil", "trouble" },
    }
  end,
}
