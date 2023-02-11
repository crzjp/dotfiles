local o = vim.o
local opt = vim.opt
local cmd = vim.cmd

o.tabstop = 4
o.shiftwidth = 4
o.expandtab = true

o.number = true
o.laststatus = 3
o.splitright = true
o.splitbelow = true
opt.fillchars = {eob = ' '}
cmd 'colorscheme photon'

o.ignorecase = true
o.smartcase = true

o.clipboard = 'unnamedplus'
o.inccommand = 'split'
o.mouse = false
