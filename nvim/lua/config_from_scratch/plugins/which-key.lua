local setup, which_key = pcall(require, "which-key")
if not setup then
    return
end

which_key.setup()
