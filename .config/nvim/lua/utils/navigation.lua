-- Navigation utilities for LSP results
-- Compatible with Neovim 0.11+ using on_list handler
local M = {}

-- State for LSP navigation
local nav_state = {
  items = nil,
  current_index = 0,
  type = nil,
}

-- Navigate through stored results (next/previous)
function M.navigate_lsp_results(direction)
  if not nav_state.items or #nav_state.items == 0 then
    vim.notify("No LSP results. Use gd/gr/gi first.", vim.log.levels.WARN)
    return
  end

  if direction == "next" then
    nav_state.current_index = (nav_state.current_index % #nav_state.items) + 1
  else
    nav_state.current_index = nav_state.current_index - 1
    if nav_state.current_index < 1 then
      nav_state.current_index = #nav_state.items
    end
  end

  local item = nav_state.items[nav_state.current_index]
  if item.filename then
    vim.cmd("edit " .. item.filename)
    vim.api.nvim_win_set_cursor(0, { item.lnum, (item.col or 1) - 1 })
    vim.notify(string.format("%s %d/%d", nav_state.type, nav_state.current_index, #nav_state.items), vim.log.levels.INFO)
  end
end

-- Custom on_list handler for LSP operations
local function make_on_list(type)
  return function(options)
    -- Store results for navigation
    nav_state.items = options.items
    nav_state.current_index = 1
    nav_state.type = type

    if #options.items == 0 then
      vim.notify("No " .. type .. " found", vim.log.levels.INFO)
    elseif #options.items == 1 then
      -- Single result: jump directly
      local item = options.items[1]
      vim.cmd("edit " .. item.filename)
      vim.api.nvim_win_set_cursor(0, { item.lnum, (item.col or 1) - 1 })
    else
      -- Multiple results: jump to first and notify
      local item = options.items[1]
      vim.cmd("edit " .. item.filename)
      vim.api.nvim_win_set_cursor(0, { item.lnum, (item.col or 1) - 1 })
      vim.notify(string.format("%d %ss found. Use ]d/[d to navigate.", #options.items, type), vim.log.levels.INFO)
    end
  end
end

-- Go to definition with navigation support
function M.goto_definition()
  vim.lsp.buf.definition({ on_list = make_on_list("definition") })
end

-- Go to references with navigation support
function M.goto_references()
  vim.lsp.buf.references(nil, { on_list = make_on_list("reference") })
end

-- Go to implementation with navigation support
function M.goto_implementation()
  vim.lsp.buf.implementation({ on_list = make_on_list("implementation") })
end

-- Simple word navigation
function M.next_word_with_definition()
  vim.cmd("normal! *")
end

function M.prev_word_with_definition()
  vim.cmd("normal! #")
end

return M
