return {
  "akinsho/bufferline.nvim",
  version = "*",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  event = "VeryLazy",
  opts = {
    options = {
      mode = "buffers",
      themable = true,
      numbers = "none",
      close_command = "bdelete! %d",
      indicator = {
        style = "icon",
        icon = "▎",
      },
      buffer_close_icon = "󰅖",
      modified_icon = "●",
      close_icon = "",
      left_trunc_marker = "",
      right_trunc_marker = "",
      max_name_length = 30,
      max_prefix_length = 15,
      tab_size = 18,
      diagnostics = "nvim_lsp",
      diagnostics_indicator = function(count, level)
        local icon = level:match("error") and " " or " "
        return " " .. icon .. count
      end,
      offsets = {
        {
          filetype = "oil",
          text = "File Explorer",
          highlight = "Directory",
          separator = true,
        },
      },
      show_buffer_icons = true,
      show_buffer_close_icons = true,
      show_close_icon = false,
      show_tab_indicators = true,
      separator_style = "thin",
      always_show_bufferline = true,
    },
  },
  keys = {
    { "<leader>bp", "<cmd>BufferLineTogglePin<CR>", desc = "Toggle pin" },
    { "<leader>bP", "<cmd>BufferLineGroupClose ungrouped<CR>", desc = "Delete non-pinned buffers" },
  },
}
