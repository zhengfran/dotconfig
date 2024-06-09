local ls = require("luasnip")
local fmta = require("luasnip.extras.fmt").fmta
local i = ls.insert_node
local r = require("luasnip.extras").rep
local s = ls.snippet
local f = ls.function_node
local d = ls.dynamic_node
local c = ls.choice_node
return {
	s(
		{ name = "Header include guard", trig = "ig" },
		fmta(
			[[
            #ifndef <NODE_1>_H
            #define <NODE_2>_H

            <NODE_4> 

            #endif /* <NODE_3>_H */
            ]],
			{
				NODE_1 = i(1, "NAME"),
				NODE_2 = r(1),
				NODE_3 = r(1),
				NODE_4 = i(2, "body"),
			}
		)
	),
}
