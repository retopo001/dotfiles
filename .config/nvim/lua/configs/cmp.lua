-- Custom cmp config: auto-trigger completions, VS Code-style Tab to accept
local cmp = require "cmp"
local default_opts = require "nvchad.configs.cmp"

-- Auto-trigger completion as you type (no manual trigger needed)
default_opts.completion = {
  completeopt = "menu,menuone,noinsert",
  autocomplete = { cmp.TriggerEvent.TextChanged, cmp.TriggerEvent.InsertEnter },
}

-- Show completion menu immediately after 1 character
default_opts.performance = {
  debounce = 0,
  throttle = 0,
}

-- Override Tab to confirm selection (VS Code style)
default_opts.mapping["<Tab>"] = cmp.mapping(function(fallback)
  if cmp.visible() then
    cmp.confirm({ behavior = cmp.ConfirmBehavior.Insert, select = true })
  elseif require("luasnip").expand_or_jumpable() then
    require("luasnip").expand_or_jump()
  else
    fallback()
  end
end, { "i", "s" })

-- Formatting override for Tailwind colorizer
local original_format = default_opts.formatting and default_opts.formatting.format
default_opts.formatting = default_opts.formatting or {}
default_opts.formatting.format = function(entry, item)
  if original_format then
    item = original_format(entry, item)
  end
  local ok, colorizer = pcall(require, "tailwindcss-colorizer-cmp")
  if ok then
    item = colorizer.formatter(entry, item)
  end
  return item
end

return default_opts
