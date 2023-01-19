function Map(mode, lhs, rhs, opts)
    local options = { noremap=true, silent=true }
    if opts then
        options = vim.tbl_extend('force', options, opts)
    end
    vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

-- Change leader to a comma
vim.g.mapleader = ','

-- Disable arrow keys
Map('', '<up>', '<nop>')
Map('', '<down>', '<nop>')
Map('', '<left>', '<nop>')
Map('', '<right>', '<nop>')

-- Map Esc to 
Map('i', '<C-c>', '<Esc>')

-- close current window
Map('n', '<C-c><C-c>', '<cmd>close<cr>')

-- Clear search highlighting with <leader> and c
Map('n', '<leader>c', ':nohl<cr>')

-- Reload configuration without restart nvim
Map('n', '<leader>r', ':so %<cr>')

-- Fast saving with <leader> and s
Map('n', '<C-s>', ':w<cr>')
Map('i', '<leader>s', '<C-c>:w<cr>')

-- Close all windows and exit from Neovim with <leader> and q
Map('n', '<leader>q', ':qa!<cr>')

-- NvimTree
Map('n', '<C-n>', ':NvimTreeToggle<cr>')            -- open/close
Map('n', '<leader>f', ':NvimTreeRefresh<cr>')       -- refresh
Map('n', '<leader>n', ':NvimTreeFindFile<cr>')      -- search file

-- Split window
Map('n', '<leader>v', ':vsplit<cr>');

-- Change split orientation
Map('n', '<leader>tk', '<C-w>t<C-w>K') -- change vertical to horizontal
Map('n', '<leader>th', '<C-w>t<C-w>H') -- change horizontal to vertical

-- Move around splits using Ctrl + {h,j,k,l}
Map('n', '<C-h>', '<C-w>h')
Map('n', '<C-j>', '<C-w>j')
Map('n', '<C-k>', '<C-w>k')
Map('n', '<C-l>', '<C-w>l')

-- Buffer explorer
Map('n', '<leader>be', ':BufExplorer<cr>')
Map('n', '<leader>be', ':ToggleBufExplorer<cr>')

--- Copy-paste
Map('v', '<leader>p', '"_dP')
Map('n', '<leader>y', '"+y')
Map('v', '<leader>y', '"+y')
Map('n', '<leader>Y', 'gg"+yG')
Map('n', '<leader>d', '"_d')
Map('v', '<leader>d', '"_d')

--- Terminal
Map('n', '<C-t>', ':Term<cr>', { noremap = true })  -- open
Map('t', '<Esc>', '<C-\\><C-n>')                    -- exit
