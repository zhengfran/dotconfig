local status, git_blame = pcall(require, "git-blame")
if not status then
	return
end

git_blame.setup()
