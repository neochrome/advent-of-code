#!/bin/sh
cat input |
#echo "R8, R4, R4, R8" |
awk '
function loc() { return x "," y; }
function abs (v) { return v < 0 ? -v : v; }
BEGIN {
	FS = ""; RS = ", "; # setup field transformation
	x = y = 0;          # starting position
	dir = 0             # facing north
	found_hq = 0
	locations[loc()] = 1;
}
$1 == "R" { dir++; }
$1 == "L" { dir--; }
dir < 0 { dir += 4; }
{
	dir %= 4;
	steps = substr($0, 2);
	while (steps > 0) {
		switch (dir) {
			case 0: y++; break; # north
			case 1: x++; break; # east
			case 2: y--; break; # south
			case 3: x--; break; # west
		}
		l = loc();
		if (l in locations) {
			found_hq = 1;
			exit 0;
		}
		locations[l] = 1;
		steps--;
	}
}
END {
	if (found_hq) {
		printf "part2: hq at x=%d, y=%d, distance=%d\n", x, y, abs(x) + abs(y);
	} else {
		printf "part2: no solution found!\n";
	}
}
'
