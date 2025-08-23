local M = {}

function M.setup()
  -- 使用 vim.fn.complete_info() 来检测补全状态
  vim.keymap.set("i", "<Tab>", function()
    local info = vim.fn.complete_info()
    if info.pum_visible == 1 then
      return "<C-n>"
    else
      return "<Tab>"
    end
  end, { expr = true })

  -- Shift-Tab
  vim.keymap.set("i", "<S-Tab>", function()
    local info = vim.fn.complete_info()
    if info.pum_visible == 1 then
      return "<C-p>"
    else
      return "<S-Tab>"
    end
  end, { expr = true })
end

return M
