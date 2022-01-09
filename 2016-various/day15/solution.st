Object subclass: Disc [
	| size position |
	Disc class [
		new: aSize position: aPosition [
			^ self new init: aSize position: aPosition.
		]
	]
	init: aSize position: aPosition [
		size := aSize.
		position := aPosition \\ aSize.
	]
	isOpen: atTime [
		^ (position + atTime) \\ size = 0.
	]
].

Object subclass: Sculpture [
	| discs |
	Sculpture class [
		new: someDiscs [
			^ self new init: someDiscs.
		]
	]
	init: someDiscs [
		discs := someDiscs.
	]
	findReleaseTime [
		| time allOpen |
		time := 0.
		[
			time := time + 1.
			allOpen := true.
			discs with: (discs keys) do: [ :disc :offset |
				allOpen := allOpen & (disc isOpen: (time + offset)).
			].
			allOpen.
		] whileFalse: [].
		^ time.
	]
].

example := Sculpture new: {
	Disc new: 5 position: 4.
	Disc new: 2 position: 1.
}.
(example findReleaseTime = 5) ifFalse: [Error signal: 'Did not got expected time of 5.'].

part1 := Sculpture new: {
	Disc new: 13 position: 10.
	Disc new: 17 position: 15.
	Disc new: 19 position: 17.
	Disc new: 7  position: 1.
	Disc new: 5  position: 0.
	Disc new: 3  position: 1.
}.
('part1: %1' bindWith: (part1 findReleaseTime)) printNl.

part2 := Sculpture new: {
	Disc new: 13 position: 10.
	Disc new: 17 position: 15.
	Disc new: 19 position: 17.
	Disc new: 7  position: 1.
	Disc new: 5  position: 0.
	Disc new: 3  position: 1.
	Disc new: 11 position: 0.
}.
('part2: %1' bindWith: (part2 findReleaseTime)) printNl.
