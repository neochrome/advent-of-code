IMAGE=python:alpine
all: build run

build:
	@docker pull $(IMAGE)

run:
	@docker run --rm -it -v "$$(pwd):/src" -w /src $(IMAGE) python solution.py

clean:
	@docker rmi $(IMAGE)
