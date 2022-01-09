IMAGE=swift:3

all: build run

build:
	@docker pull $(IMAGE)

run:
	@docker run --rm -it -v "$$(pwd):/src" -w /src $(IMAGE) swift solution.swift

clean:
	@docker rmi $(IMAGE)
