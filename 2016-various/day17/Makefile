IMAGE=crystallang/crystal:0.23.1

all: build run

build:
	@docker pull $(IMAGE)

run:
	@docker run --rm -it -v "$$(pwd):/src" -w /src $(IMAGE) crystal run ./solution.cr

clean:
	@docker rmi $(IMAGE)
