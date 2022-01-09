use strict;
use warnings;

sub assert {
	my($check, @examples) = @_;
	foreach (@examples) {
		my @i = $_;
		my $ip = $i[0][0];
		my $result = $i[0][1];
		$check->($ip) == $result or die "failed: $ip\n";
	}
}

sub supports_tls {
	return
		$_[0] =~ /(\w)((?!\1)\w)\2\1/               # abba
		&& $_[0] !~ /\[\w*(\w)((?!\1)\w)\2\1\w*\]/; # no [abba]
}
assert(\&supports_tls, (
	['abba[mnop]qrst',       1],
	['abcd[bddb]xyyx',       0],
	['abcd[xbddbz]xyyx',     0],
	['aaaa[qwer]tyui',       0],
	['ioxxoj[asdfgh]zxcvbn', 1],
));

sub supports_ssl {
	return $_[0] =~ /\[\w*(\w)(?!\1)(\w)\1\w*\](?=.*\2\1\2(?!\w*\]))/ # [bab] then aba
		|| $_[0] =~ /(\w)(?!\1)(\w)\1(?!\w*\])(?=.*\[\w*\2\1\2\w*\])/   # aba then [bab]
}
assert(\&supports_ssl, (
	['aba[bab]xyz',   1],
	['xyx[xyx]xyx',   0],
	['xyx[xyx]xyx',   0],
	['aaa[kek]eke',   1],
	['zazbz[bzb]cdb', 1],
	['aba[xyx]bab[yxy][aba]',1],
));

open my $f, './input' or die 'Could not read input';
my $part1 = 0;
my $part2 = 0;
while (my $ip = <$f>) {
	$part1++ if supports_tls($ip);
	$part2++ if supports_ssl($ip);
}
close $f;
print "part1: $part1\npart2: $part2\n";
