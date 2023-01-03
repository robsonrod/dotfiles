local function map(mode, lhs, rhs, opts)
    local options = { noremap=true, silent=true }
    if opts then
        options = vim.tbl_extend('force', options, opts)
    end
    vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

-- Change leader to a comma
vim.g.mapleader = ' '

-- Disable arrow keys
map('', '<up>', '<nop>')
map('', '<down>', '<nop>')
map('', '<left>', '<nop>')
map('', '<right>', '<nop>')

-- Map Esc to 
map('i', '<C-c>', '')
map('i', '<C-c>', '<Esc>')

-- close current window
map('n', '<C-c><C-c>', '<cmd>close<cr>')

-- Clear search highlighting with <leader> and c
map('n', '<leader>c', ':nohl<cr>')

-- Reload configuration without restart nvim
map('n', '<leader>r', ':so %<cr>')

-- Fast saving with <leader> and s
map('n', '<C-s>', ':w<cr>')

-- Close all windows and exit from Neovim with <leader> and q
map('n', '<leader>q', ':qa!<cr>')

-- NvimTree
map('n', '<C-n>', ':NvimTreeToggle<cr>')            -- open/close
map('n', '<leader>f', ':NvimTreeRefresh<cr>')       -- refresh
map('n', '<leader>n', ':NvimTreeFindFile<cr>')      -- search file

-- Split window
map('n', '<leader>v', ':vsplit<cr>');

-- Change split orientation
map('n', '<leader>tk', '<C-w>t<C-w>K') -- change vertical to horizontal
map('n', '<leader>th', '<C-w>t<C-w>H') -- change horizontal to vertical

-- Move around splits using Ctrl + {h,j,k,l}
map('n', '<C-h>', '<C-w>h')
map('n', '<C-j>', '<C-w>j')
map('n', '<C-k>', '<C-w>k')
map('n', '<C-l>', '<C-w>l')

-- Buffer explorer
map('n', '<leader>be', ':BufExplorer<cr>')
map('n', '<leader>be', ':ToggleBufExplorer<cr>')

--- Copy-paste
map('v', '<leader>p', '"_dP')
map('n', '<leader>y', '"+y')
map('v', '<leader>y', '"+y')
map('n', '<leader>Y', 'gg"+yG')
map('n', '<leader>d', '"_d')
map('v', '<leader>d', '"_d')

