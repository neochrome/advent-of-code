func groupsOf(_ tiles: String) -> AnySequence<String> {
	let padded = "." + tiles + "."
		let size = 3
		return AnySequence<String>(sequence(
			first: (padded.startIndex, padded.index(padded.startIndex, offsetBy: size)),
			next: { (from, to) in
				let from = padded.index(after: from)
				if let to = padded.index(from, offsetBy: size, limitedBy: padded.endIndex) { return (from, to) }
				return nil
			}
		).map{ (from, to) in padded[from..<to] })
}

func tileType(_ group: String) -> String {
	return ["^^.",".^^","^..","..^"].contains(group)
		? "^"
		: "."
}

let isSafe = { (tile: Character) in tile == "." }

func step(_ tiles: String, _ steps: Int) -> Int {
	var safeCount = 0
	var nextTiles = tiles
	var stepsLeft = steps
	while stepsLeft > 0 {
		safeCount += nextTiles.characters.filter(isSafe).count
		nextTiles = groupsOf(nextTiles).map(tileType).joined()
		stepsLeft -= 1
	}
	return safeCount
}

print("example: \(step(".^^.^.^^^^", 10))")

let input = ".^^.^^^..^.^..^.^^.^^^^.^^.^^...^..^...^^^..^^...^..^^^^^^..^.^^^..^.^^^^.^^^.^...^^^.^^.^^^.^.^^.^."
print("part1: \(step(input, 40))")
print("part2: \(step(input, 400000))")
