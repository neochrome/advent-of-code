const examples = [
	"eedadn",
	"drvtee",
	"eandsr",
	"raavrd",
	"atevrs",
	"tsrnev",
	"sdttsa",
	"rasrtv",
	"nssdts",
	"ntnada",
	"svetve",
	"tesnvt",
	"vntsnd",
	"vrdear",
	"dvrsen",
	"enarar",
];

function countLetters (lines) {
	let freqs = {};
	for (let line of lines) {
		for (let i = 0; i < line.length; i++) {
			let ch = line.charAt(i);
			freqs[i] = (freqs[i] || {});
			freqs[i][ch] = (freqs[i][ch] || 0) + 1;
		}
	}
	return Object
		.keys(freqs)
		.map(i => freqs[i])
		.map(f => {
			let sorted = Object.keys(f)
			.map(ch => ({ ch, f: f[ch] }))
			.sort((a,b) => a.f - b.f);
			return { min: sorted[0].ch, max: sorted[sorted.length-1].ch };
		});
}

const assert = require('assert');
assert.equal(countLetters(examples).reduce((str, f) => str + f.max, ""), "easter", "part1");
assert.equal(countLetters(examples).reduce((str, f) => str + f.min, ""), "advent", "part2");

const input = require('fs').readFileSync('./input', 'utf8').split('\n');
let part1 = countLetters(input).reduce((str, f) => str + f.max, "");
let part2 = countLetters(input).reduce((str, f) => str + f.min, "");
console.log('part1: %s\npart2: %s', part1, part2);
