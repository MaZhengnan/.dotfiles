-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here
local uname = vim.loop.os_uname()

local sysname = uname.sysname

local release = uname.release

-- helper: 检查命令是否可用

local function has(cmd)
  return vim.fn.executable(cmd) == 1
end

if sysname == "Darwin" then
  -- macOS

  vim.g.clipboard = {

    name = "macOS-clipboard",

    copy = { ["+"] = "pbcopy", ["*"] = "pbcopy" },

    paste = { ["+"] = "pbpaste", ["*"] = "pbpaste" },

    cache_enabled = 0,
  }
elseif sysname == "Linux" then
  if release:match("microsoft") or release:match("WSL") then
    -- Linux + WSL
    vim.g.clipboard = {

      name = "win32yank-wsl",
      copy = {
        ["+"] = "win32yank -i --crlf",
        ["*"] = "win32yank -i --crlf",
      },

      paste = {
        ["+"] = "win32yank -o --lf",
        ["*"] = "win32yank -o --lf",
      },
      cache_enabled = 0,
    }
  elseif has("wl-copy") and has("wl-paste") then
    -- 原生 Linux (Wayland)

    vim.g.clipboard = {

      name = "Wayland-clipboard",

      copy = { ["+"] = "wl-copy", ["*"] = "wl-copy" },

      paste = { ["+"] = "wl-paste", ["*"] = "wl-paste" },

      cache_enabled = 0,
    }
  elseif has("xclip") then
    -- 原生 Linux (X11)

    vim.g.clipboard = {

      name = "X11-clipboard",

      copy = {

        ["+"] = "xclip -selection clipboard",

        ["*"] = "xclip -selection primary",
      },

      paste = {

        ["+"] = "xclip -selection clipboard -o",

        ["*"] = "xclip -selection primary -o",
      },

      cache_enabled = 0,
    }
  end
end

vim.opt.clipboard = "unnamedplus"

-- Tab 补全配置

local function tab_complete()
  if vim.fn.pumvisible() == 1 then
    -- 如果补全菜单可见，使用 Tab 选择下一个项目
    return "<C-n>"
  elseif vim.fn.call("vsnip#available", { 1 }) == 1 then
    -- 如果 vsnip 可用，展开代码片段
    return "<Plug>(vsnip-expand-or-jump)"
  else
    -- 否则插入普通的 Tab
    return "<Tab>"
  end
end

local function shift_tab_complete()
  if vim.fn.pumvisible() == 1 then
    -- 如果补全菜单可见，使用 Shift-Tab 选择上一个项目
    return "<C-p>"
  else
    -- 否则向后缩进
    return "<S-Tab>"
  end
end

-- 设置 Tab 键映射
vim.keymap.set("i", "<Tab>", "v:lua.tab_complete()", { expr = true, noremap = true })
vim.keymap.set("i", "<S-Tab>", "v:lua.shift_tab_complete()", { expr = true, noremap = true })
