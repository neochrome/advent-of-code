IMAGE=golang:1.9-alpine

all: build run

build:
	@docker pull $(IMAGE)

run:
	@docker run --rm -it -v "$$(pwd):/src" -w /src $(IMAGE) /bin/sh -c 'go run ./main.go'

clean:
	@docker rmi $(IMAGE)
