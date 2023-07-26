-- Automatically install packer
local ensure_packer = function()
    local fn = vim.fn
    local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
    if fn.empty(fn.glob(install_path)) > 0 then
        fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
        vim.cmd [[packadd packer.nvim]]
        return true
    end
    return false
end

local packer_bootstrap = ensure_packer()

vim.cmd [[
augroup packer_user_config
autocmd!
autocmd BufWritePost packer_init.lua source <afile> | PackerSync
augroup end
]]

local status_ok, packer = pcall(require, 'packer')
if not status_ok then
    return
end

return packer.startup(function(use)
    use 'wbthomason/packer.nvim'

    use {
        'nvim-tree/nvim-tree.lua',
        requires = {
            'nvim-tree/nvim-web-devicons', -- optional, for file icons
        },
        tag = 'nightly' -- optional, updated every week. (see issue #1193)
    }

    use {
        'nvim-telescope/telescope.nvim', tag = '0.1.0',
        -- or                            , branch = '0.1.x',
        requires = { {'nvim-lua/plenary.nvim'} }
    }

    use 'nvim-lua/plenary.nvim'

    use 'windwp/nvim-spectre'

    use 'jlanzarotta/bufexplorer'

    use {"windwp/nvim-autopairs", as = 'autopairs'}

    use {'navarasu/onedark.nvim', as = 'onedark' }
    use {'Mofiqul/dracula.nvim', as = 'dracula' }
    use {'catppuccin/nvim', as = 'catppuccin' }
    use {'p00f/nvim-ts-rainbow'}
    use {'folke/tokyonight.nvim', as = 'tokyonight'}

    use {'nvim-lualine/lualine.nvim', requires = { 'kyazdani42/nvim-web-devicons', opt = true }}
    use {'goolord/alpha-nvim', requires = { 'kyazdani42/nvim-web-devicons' }}

    use({'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'})
    use('nvim-treesitter/playground')

    use('mbbill/undotree')
    use('tpope/vim-fugitive')
    use('scrooloose/nerdcommenter')

    use {
        'VonHeikemen/lsp-zero.nvim',
        requires = {
            -- LSP Support
            {'neovim/nvim-lspconfig'},
            {'williamboman/mason.nvim'},
            {'williamboman/mason-lspconfig.nvim'},

            -- Autocompletion
            {'hrsh7th/nvim-cmp'},
            {'hrsh7th/cmp-buffer'},
            {'hrsh7th/cmp-path'},
            {'saadparwaiz1/cmp_luasnip'},
            {'hrsh7th/cmp-nvim-lsp'},
            {'hrsh7th/cmp-nvim-lua'},

            -- Snippets
            {'L3MON4D3/LuaSnip'},
            {'rafamadriz/friendly-snippets'},
        }
    }

    use {'Olical/conjure', tag='v4.45.0'}
    use {'tpope/vim-dispatch'}
    use {'clojure-vim/vim-jack-in'}
    use {'radenling/vim-dispatch-neovim'}

    use {'p00f/clangd_extensions.nvim'}
    use {'nvim-orgmode/orgmode', as='orgmode'}
    --
    --
    -- Automatically set up your configuration after cloning packer.nvim
    -- Put this at the end after all plugins
    if packer_bootstrap then
        require('packer').sync()
    end
end)
