vim.g.mapleader = " "

local keymap = vim.keymap --for conciseness

--general keymaps
keymap.set("i", "jj", "<ESC>")

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

-- plugin keymaps

-- vim-maximizer
keymap.set("n", "<C-w>m", ":MaximizerToggle<CR>") --go to previous tab

--nvim -tree
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
-- just to tab to navigate instead
-- keymap.set("n", "<M-h>", ":BufferLineCyclePrev<CR>")
-- keymap.set("n", "<M-h>", ":BufferLineCyclePrev<CR>")
-- keymap.set("n", "<M-l>", ":BufferLineCycleNext<CR>")
-- keymap.set("n", "<M-l>", ":BufferLineCycleNext<CR>")
