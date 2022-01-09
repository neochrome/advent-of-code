IMAGE=nacyot/smalltalk-gnu:apt

all: build run

build:
	@docker pull $(IMAGE)

run:
	@docker run --rm -it -v "$$(pwd):/src" -w /src $(IMAGE) gst ./solution.st

clean:
	@docker rmi $(IMAGE)
