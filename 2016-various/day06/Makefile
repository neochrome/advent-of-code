IMAGE=node:7-alpine

all: build run

build:
	@docker pull $(IMAGE)

run:
	@docker run --rm -it -v "$$(pwd):/src" -w /src $(IMAGE) node solution.js

clean:
	@docker rmi $(IMAGE)
