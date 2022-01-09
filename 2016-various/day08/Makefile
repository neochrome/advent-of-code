IMAGE=ruby:2.4-alpine

all: build run

build:
	@docker pull $(IMAGE)

run:
	@docker run --rm -it -v "$$(pwd):/src" -w /src $(IMAGE) ruby solution.rb

clean:
	@docker rmi $(IMAGE)
