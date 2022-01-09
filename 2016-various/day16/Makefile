IMAGE=clojure:alpine

all: build run

build:
	@docker pull $(IMAGE)

shell:
	@docker run --rm -it -v "$$(pwd):/src" -w /src $(IMAGE) /bin/sh

repl:
	@docker run --rm -it -v "$$(pwd):/src" -w /src $(IMAGE) lein repl 

run:
	@docker run --rm -it -v "$$(pwd):/src" -w /src $(IMAGE) /bin/sh -c "echo -e '(load-file \"./solution.clj\")' | lein repl"

clean:
	@docker rmi $(IMAGE)
