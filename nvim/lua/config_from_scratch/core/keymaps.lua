vim.g.mapleader = " "

local keymap = vim.keymap --for conciseness

--general keymaps
keymap.set("i", "fd", "<ESC>")

--reload vim config without quitting
keymap.set("n", "<leader>sv", ":source $MYVIMRC<CR>")

-- remove search highlight
keymap.set("n", "<leader>nh", ":nohl<CR>")

-- x does not copy character into register
keymap.set("n", "x", '"_x')

--tabs
keymap.set("n", "<leader>to", ":tabnew<CR>") --open new tab
keymap.set("n", "<leader>tq", ":tabclose<CR>") --close current tab
keymap.set("n", "<leader>tn", ":tabnext<CR>") -- go to next tab
keymap.set("n", "<leader>tp", ":tabprevious<CR>") --go to previous tab

--same configuration to navigate splits as emacs
keymap.set("n", "<C-w>c", "<C-w>q") -- quit

-- plugin keymaps

-- vim-maximizer
keymap.set("n", "<C-w>m", ":MaximizerToggle<CR>") -- Vim-maximizer

--nvim-tree
keymap.set("n", "<leader>op", ":NvimTreeToggle<CR>")
keymap.set("n", "<leader>oc", ":NvimTreeCollapse<CR>")
keymap.set("n", "<leader>of", ":NvimTreeFindFile<CR>")

--telescope
keymap.set("n", "<leader>ff", "<cmd>Telescope find_files theme=dropdown<CR>") --find file
keymap.set("n", "<leader>fg", "<cmd>Telescope live_grep theme=dropdown<CR>") --find string
keymap.set("n", "<leader>fw", "<cmd>Telescope grep_string theme=dropdown<CR>") --find current word
keymap.set("n", "<leader>fb", "<cmd>Telescope buffers theme=dropdown<CR>") --find buffer
keymap.set("n", "<leader>fh", "<cmd>Telescope help_tags theme=dropdown<CR>") --find help
keymap.set("n", "<leader>fo", "<cmd>Telescope oldfiles theme=dropdown<CR>") --find recently open files

--bufferline
keymap.set("n", "<M-h>", ":BufferLineCyclePrev<CR>")
keymap.set("n", "<M-l>", ":BufferLineCycleNext<CR>")
keymap.set("n", "<M-1>", ":BufferLineGoToBuffer 1<CR>")
keymap.set("n", "<M-2>", ":BufferLineGoToBuffer 2<CR>")
keymap.set("n", "<M-3>", ":BufferLineGoToBuffer 3<CR>")
keymap.set("n", "<M-4>", ":BufferLineGoToBuffer 4<CR>")
keymap.set("n", "<M-5>", ":BufferLineGoToBuffer 5<CR>")
keymap.set("n", "<M-6>", ":BufferLineGoToBuffer 6<CR>")
keymap.set("n", "<M-7>", ":BufferLineGoToBuffer 7<CR>")
keymap.set("n", "<M-8>", ":BufferLineGoToBuffer 8<CR>")
keymap.set("n", "<M-9>", ":BufferLineGoToBuffer 9<CR>")

--null-ls formatter
keymap.set("n", "<leader>fr", "<cmd>lua vim.lsp.buf.format()<CR>") -- got to declaration

--toggleterm
keymap.set("n", "<leader>tt", "<cmd>exe v:count1 . 'ToggleTerm'<CR>") --open toggle term
keymap.set("n", "<leader>tg", "<cmd>lua _LAZYGIT_TOGGLE()<CR>") --open lazy git

--osc52
keymap.set('n', '<leader>c', require('osc52').copy_operator, {expr = true})
keymap.set('n', '<leader>cc', '<leader>c_', {remap = true})
keymap.set('v', '<leader>c', require('osc52').copy_visual)
