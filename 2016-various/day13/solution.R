wall_at <- function(x,y) {
	if (x < 0 || y < 0) TRUE
	else {
		b <- intToBits(x*x + 3*x + 2*x*y + y + y*y + 1364)
		length(b[b==1]) %% 2 != 0
	}
}

point <- function(x,y) { data.frame(x,y) }
move <- function(steps,loc) { data.frame(steps,loc) }
to_move <- function(m) { data.frame(m) }
hash <- function(loc) { sprintf("%d,%d", loc$x, loc$y) }

directions = lapply(
	list(point(-1,0),point(1,0),point(0,-1),point(0,1)),
	function(o) { move(1,o) }
)
available_moves <- function(m) {
	Filter(
		function(m){ !wall_at(m$x,m$y) },
		lapply(directions, function(o) { m+o })
	)
}

part1 <- function() {
	target <- point(31,39)
	start <- move(0,point(1,1))
	visited <- c(hash(start))
	moves <- available_moves(start)
	while (length(moves) > 0) {
		m <- to_move(moves[1])
		moves <- moves[-1]
		if (all(target %in% m)) {
			print(sprintf("part1: %d",m$steps))
			break
		}
		h <- hash(m)
		if (h %in% visited) next
		visited <- c(visited, h)
		moves <- c(moves, available_moves(m))
	}
	if (length(moves) == 0) print("part1: no solution found")
}

part2 <- function() {
	visit <- function(visited,start) {
		if (start$steps > 50) return(visited)
		h <- hash(start)
		if (h %in% visited) return(visited)
		visited_updated <- c(visited, h)
		for (m in available_moves(start)) {
			visited <- union(visited, visit(visited_updated, m))
		}
		visited
	}
	start <- move(0, point(1, 1))
	reached <- length(visit(c(), start))
	print(sprintf("part2: %d", reached))
}

part1()
part2()
