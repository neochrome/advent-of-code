<?php
$n = 0;
#$id = "abc";
$id = "cxdnnyjw";
$part1 = "";
$part2 = [];
while (sizeof($part2) < 8) {
	$idn = $id.$n;
	$hash = md5($idn);
	if (substr($hash, 0, 5) === "00000") {
		printf("%s\n", $hash);
		if (strlen($part1) < 8) {
			$part1 .= substr($hash, 5, 1);
		}
		$idx = intval(substr($hash, 5, 1), 16);
		if ($idx < 8 && !isset($part2[$idx])) {
			$part2[$idx] = substr($hash, 6, 1);
		}
	}
	$n++;
}
printf("part1: %s\n", $part1);
ksort($part2);
printf("part2: %s\n", join($part2, ""));
?>
