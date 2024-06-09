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
		{ name = "C Function Comment", trig = "/**" },
		fmta(
			[[
            /**
            println!("<NODE_1> = {:?}", <NODE_2>);
            **/
    println!("<NODE_1> = {:?}", <NODE_2>);
    ]],
			{
				NODE_1 = i(1, "NAME"),
				NODE_2 = r(1),
			}
		)
	),
}
