local status_ok = pcall(require, 'dracula')
if not status_ok then
    return
end

require("dracula").setup({
})

require("catppuccin").setup({
    flavour = "macchiato", -- latte, frappe, macchiato, mocha
    background = { -- :h background
    light = "latte",
    dark = "macchiato",
},
transparent_background = false,
term_colors = true,
no_italic = false, -- Force no italic
no_bold = false, -- Force no bold
styles = {
    comments = { "italic" },
    conditionals = {},
    loops = {},
    functions = { "bold" },
    keywords = { "bold" },
    strings = {},
    variables = { "bold"},
    numbers = {},
    booleans = {},
    properties = {},
    types = {},
    operators = {},
},
color_overrides = {
},
custom_highlights = {},
integrations = {
    cmp = true,
    gitsigns = true,
    nvimtree = true,
    telescope = true,
    notify = false,
    mini = false,
    -- For more plugins integrations please scroll down (https://github.com/catppuccin/nvim#integrations)
},
})

require('onedark').setup {
    -- styles: dark, darker, cool, deep, warm, warmer, light
    style = 'darker',
    colors = { fg = '#b2bbcc' }, --default: #a0a8b7
}

require("tokyonight").setup({
    -- use the night style
    style = "moon",
    -- disable italic for functions
    styles = {
        comments = { italic = false },
        keywords = { italic = false },
        functions = {}
    },
})

require('catppuccin').load()
