IMAGE=php:7.1-alpine

all: build run

build:
	@docker pull $(IMAGE)

run:
	@docker run --rm -it -v "$$(pwd):/src" -w /src $(IMAGE) php solution.php

clean:
	@docker rmi $(IMAGE)
