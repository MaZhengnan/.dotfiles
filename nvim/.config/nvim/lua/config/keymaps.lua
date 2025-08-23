-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here
--
--

local keymap = vim.keymap.set
local opts = { noremap = true, silent = true }

-- jk 退出插入模式
keymap("i", "jk", "<ESC>", { desc = "Exit insert mode" })
keymap("i", "kj", "<ESC>", { desc = "Exit insert mode" }) -- 可选：添加 kj 作为备用
-- 也可以配置其他模式
keymap("v", "jk", "<ESC>", { desc = "Exit visual mode" })
keymap("c", "jk", "<C-c>", { desc = "Exit command mode" })
