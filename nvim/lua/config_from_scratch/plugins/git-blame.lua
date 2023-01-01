local setup, gitblame = pcall(require, "git-blame")
if not setup then
	return
end

gitblame.setup()
