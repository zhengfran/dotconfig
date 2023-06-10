return {
    "ggandor/leap.nvim", --leap

    lazy = false, -- it lazyload itself
    config = function()
        require("leap").set_default_keymaps()
    end,
}
