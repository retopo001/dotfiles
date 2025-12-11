-- harpoon2: Fast file navigation by ThePrimeagen
-- Mark frequently used files and jump to them instantly
return {
  "ThePrimeagen/harpoon",
  branch = "harpoon2",
  dependencies = { "nvim-lua/plenary.nvim" },
  config = function()
    local harpoon = require("harpoon")
    harpoon:setup({
      settings = {
        save_on_toggle = true,
        sync_on_ui_close = true,
      },
    })

    -- Mark current file
    vim.keymap.set("n", "<leader>hm", function()
      harpoon:list():add()
      vim.notify("󱡅  Marked file", vim.log.levels.INFO)
    end, { desc = "Harpoon mark file" })

    -- Toggle quick menu
    vim.keymap.set("n", "<leader>hh", function()
      harpoon.ui:toggle_quick_menu(harpoon:list())
    end, { desc = "Harpoon quick menu" })

    -- Navigate to marked files (1-9)
    for i = 1, 9 do
      vim.keymap.set("n", string.format("<leader>h%d", i), function()
        harpoon:list():select(i)
      end, { desc = string.format("Harpoon to file %d", i) })
    end

    -- Navigate next/prev in harpoon list
    vim.keymap.set("n", "<leader>hn", function()
      harpoon:list():next()
    end, { desc = "Harpoon next" })

    vim.keymap.set("n", "<leader>hp", function()
      harpoon:list():prev()
    end, { desc = "Harpoon prev" })
  end,
}

