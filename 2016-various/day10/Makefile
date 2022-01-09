IMAGE=swipl/swipl:7.5.0

all: build run

build:
	@docker pull $(IMAGE)

run:
	@docker run --rm -it -v "$$(pwd):/src" -w /src $(IMAGE) swipl -s solution.pl -g test,main,halt -t 'halt(1)'

shell:
	@docker run --rm -it -v "$$(pwd):/src" -w /src $(IMAGE)

clean:
	@docker rmi $(IMAGE)
