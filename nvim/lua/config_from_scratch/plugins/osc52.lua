local status, osc52 = pcall(require, "osc52")
if not status then
	return
end

osc52.setup {
  max_length = 0,      -- Maximum length of selection (0 for no limit)
  silent     = false,  -- Disable message on successful copy
  trim       = false,  -- Trim surrounding whitespaces before copy
}
