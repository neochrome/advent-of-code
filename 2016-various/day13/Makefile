IMAGE=r-base:3.3.3

all: build run

build:
	@docker pull $(IMAGE)

run:
	@docker run --rm -it -v "$$(pwd):/src" -w /src $(IMAGE) Rscript solution.R

shell:
	@docker run --rm -it -v "$$(pwd):/src" -w /src $(IMAGE) R

clean:
	@docker rmi $(IMAGE)
