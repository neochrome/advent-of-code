#include <assert.h>
#include <stdint.h>
#include <stdio.h>

#define true 1
#define false 0
// high enough primes to avoid collisions
#define MOVES_SIZE 15485863
#define SEEN_SIZE 15485863

typedef enum {
	Polonium   = 0b00000001,
	Thulium    = 0b00000010,
	Promethium = 0b00000100,
	Ruthenium  = 0b00001000,
	Cobalt     = 0b00010000,
	Elerium    = 0b00100000,
	Dilithium  = 0b01000000,
	All        = 0b01111111,
	None       = 0b00000000,
} parts_t;

typedef uint16_t floor_t;
typedef uint64_t hash_t;
typedef union {
	floor_t floors[4];
	hash_t hash;
} state_t;
const floor_t Elevator = 0b1000000000000000;

floor_t rtg(parts_t p) { return p << 8u; }
floor_t chip(parts_t p) { return p; }
int is_safe(floor_t floor) {
	floor &= ~Elevator;
	floor_t rtgs = floor >> 8u;
	floor_t chips = floor & All;
	return (rtgs == None) || (((rtgs ^ All) & chips) == None);
}

typedef struct {
	state_t state;
	int steps;
} move_t;

move_t moves[MOVES_SIZE];
int first, last;

void push(move_t m) {
	moves[last] = m;
	last = ++last % MOVES_SIZE;
}

move_t pop() {
	assert(first != last);
	move_t m = moves[first];
	first = ++first % MOVES_SIZE;
	return m;
}

int has_reached(state_t s, state_t goal) {
	return
		s.floors[0] == goal.floors[0] &&
		s.floors[1] == goal.floors[1] &&
		s.floors[2] == goal.floors[2] &&
		s.floors[3] == goal.floors[3];
}

int is_unsafe(state_t s) {
	return !(
		is_safe(s.floors[0]) &&
		is_safe(s.floors[1]) &&
		is_safe(s.floors[2]) &&
		is_safe(s.floors[3])
	);
}

int elevator_floor(state_t s) {
	for(int i = 0; i < 4; i++) {
		if (s.floors[i] & Elevator) return i;
	}
	assert(false);
}

hash_t hash_for(state_t s) {
	return s.hash;
}

hash_t seen[SEEN_SIZE];
int seen_count = 0;
int has_been_seen(state_t s) {
	hash_t hash = hash_for(s);
	for (int off = 0; off < 10; off++){
		int i = (hash % SEEN_SIZE + off) % SEEN_SIZE;
		if (seen[i] == hash) return true;
		if (seen[i] == 0) {
			seen[i] = hash;
			seen_count++;
			return false;
		}
	}
}
void reset_seen() {
	seen_count = 0;
	for (int i = 0; i < SEEN_SIZE; i++)
		seen[i] = 0;
}

state_t *possible_states_from(state_t s, int *found) {
	static state_t possible[100];
	int current_floor = elevator_floor(s);
	// find parts on current floor
	int fmax = 0;
	floor_t on_floor[14];
	for (int i = 0; i < 15; i++) {
		floor_t part = 1 << i;
		if (s.floors[current_floor] & part) on_floor[fmax++] = part;
	}
	// find all possible states going up, then down
	int p = 0;
	int direction = 1;
	do {
		int new_floor = current_floor + direction;
		direction *= -1;
		if (new_floor < 0 || new_floor > 3) continue;
		// two parts
		for (int i = 0; i < fmax - 1; i++) {
			for (int j = i + 1; j < fmax; j++) {
				possible[p] = s;
				floor_t parts = Elevator | on_floor[i] | on_floor[j];
				possible[p].floors[current_floor] &= ~parts; // remove from current floor
				possible[p].floors[new_floor] |= parts; // set on new floor
				if (is_unsafe(possible[p])) continue;
				p++;
			}
		}
		// single part
		for (int i = 0; i < fmax; i++) {
			possible[p] = s;
			floor_t parts = Elevator | on_floor[i];
			possible[p].floors[current_floor] &= ~parts; // remove from current floor
			possible[p].floors[new_floor] |= parts; // set on new floor
			if (is_unsafe(possible[p])) continue;
			p++;
		}
	} while (direction < 0);

	*found = p;
	return possible;
}

void push_moves_from(move_t move) {
	move_t new_move;
	int found;
	state_t *possible = possible_states_from(move.state, &found);
	for (int p = 0; p < found; p++) {
		if (has_been_seen(possible[p])) continue;
		new_move.state = possible[p];
		new_move.steps = move.steps + 1;
		push(new_move);
	}
}

int solve(state_t initial, state_t goal) {
	assert(!is_unsafe(initial));
	assert(!is_unsafe(goal));
	first = last = 0; //reset queue
	reset_seen();
	int iter = 0;

	move_t move;
	move.state = initial;
	move.steps = 0;
	// load with initial moves
	push_moves_from(move);

	while (first != last) {
		move = pop();
		if (has_reached(move.state, goal)) return move.steps;
		push_moves_from(move);
	}
	printf("no solution found!\n");
	assert(false);
}

void run_tests () {
	assert(is_safe(Elevator));
	assert(is_safe(None));
	assert(is_safe(rtg(Polonium)));
	assert(is_safe(Elevator | rtg(Polonium)));
	assert(is_safe(chip(Polonium)));
	assert(is_safe(Elevator | chip(Polonium)));
	assert(is_safe(rtg(Cobalt) | chip(Cobalt)));
	assert(!is_safe(rtg(Polonium) | chip(Cobalt)));
	assert(!is_safe(Elevator | rtg(Polonium) | chip(Cobalt)));

	state_t s;
	s.floors[0] = Elevator | chip(Polonium | Thulium);
	s.floors[1] = rtg(Polonium);
	s.floors[2] = rtg(Thulium);
	s.floors[3] = None;
	state_t g;
	g.floors[0] = None;
	g.floors[1] = None;
	g.floors[2] = None;
	g.floors[3] = Elevator | chip(Polonium | Thulium) | rtg(Polonium | Thulium);
	assert(!has_reached(s, g));

	printf("test: %d\n", solve(s, g));
}

void run_part1 () {
	floor_t all_parts = Polonium|Thulium|Promethium|Ruthenium|Cobalt;
	state_t s;
	s.floors[0] = Elevator | rtg(all_parts) | chip(Thulium|Ruthenium|Cobalt);
	s.floors[1] = chip(Polonium|Promethium);
	s.floors[2] = None;
	s.floors[3] = None;
	state_t g;
	g.floors[0] = None;
	g.floors[1] = None;
	g.floors[2] = None;
	g.floors[3] = Elevator | rtg(all_parts) | chip(all_parts);
	printf("part1: %d\n", solve(s, g));
}

void run_part2 () {
	floor_t all_parts = Polonium|Thulium|Promethium|Ruthenium|Cobalt|Dilithium|Elerium;
	state_t s;
	s.floors[0] = Elevator | rtg(all_parts) | chip(Thulium|Ruthenium|Cobalt|Dilithium|Elerium);
	s.floors[1] = chip(Polonium|Promethium);
	s.floors[2] = None;
	s.floors[3] = None;
	state_t g;
	g.floors[0] = None;
	g.floors[1] = None;
	g.floors[2] = None;
	g.floors[3] = Elevator | rtg(all_parts) | chip(all_parts);
	printf("part2: %d\n", solve(s, g));
}

int main () {
	run_tests();
	run_part1();
	run_part2();
	return 0;
}
