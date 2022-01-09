#!/bin/sh
cat input |
#echo "R8, R4, R4, R8" |
awk '
function abs (v) { return v < 0 ? -v : v }
BEGIN {
	FS = ""; RS = ", " # setup field transformation
	x = 0; y = 0       # starting position
	dir = 0            # facing north
}
$1 == "R" { dir++; }
$1 == "L" { dir--; }
dir < 0 { dir += 4; }
{
	dir %= 4;
	steps = substr($0, 2);
}
dir == 0 { y += steps; } # north
dir == 1 { x += steps; } # east
dir == 2 { y -= steps; } # south
dir == 3 { x -= steps; } # west
END {
	printf "part1: hq at x=%d, y=%d, distance=%d\n", x, y, abs(x) + abs(y);
}
'
