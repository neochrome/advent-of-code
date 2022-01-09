function decompressed_length(str, recurse)
	function count (c, s)
		local ch = s:sub(1,1)
		if ch == "" then return c
		elseif ch == "(" then
			local _,eom,size,repeats = s:find("(%d+)x(%d+)%)")
			c = c + repeats * (recurse and count(0, s:sub(eom + 1, eom + size)) or size)
			return count(c, s:sub(eom + size + 1))
		else
			return count(c + (ch ~= "\n" and 1 or 0), s:sub(2))
		end
	end
	return count(0, str)
end

function verify1(str, expected)
	local actual = decompressed_length(str, false)
	if actual == expected then return end
	print(str, "expected ", expected, " was ", actual)
	assert(false)
end
function verify2(str, expected)
	local actual = decompressed_length(str, true)
	if actual == expected then return end
	print(str, "expected ", expected, " was ", actual)
	assert(false)
end

verify1("ADVENT", 6)
verify1("A(1x5)BC", 7)

verify2("ADVENT", 6)
verify2("(3x3)XYZ", 9)
verify2("X(8x2)(3x3)ABCY", 20)
verify2("(27x12)(20x12)(13x14)(7x10)(1x12)A", 241920)

io.input("./input")
line = io.read("*all")
print("part1: ", decompressed_length(line, false))
print("part2: ", decompressed_length(line, true))
