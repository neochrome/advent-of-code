include ../targets.mk
IMAGE=aoc2016:day09

run:
	@docker run --rm -it -v "$$(pwd):/src" -w /src $(IMAGE) lua solution.lua
