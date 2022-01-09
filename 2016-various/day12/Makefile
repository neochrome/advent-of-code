IMAGE=nacyot/fortran-gfortran:apt

all: build run

build:
	@docker pull $(IMAGE)

run:
	@docker run --rm -it -v "$$(pwd):/src" -w /src $(IMAGE) /bin/sh -c 'gfortran -o /tmp/solution solution.f && rm subs.mod && /tmp/solution'

shell:
	@docker run --rm -it -v "$$(pwd):/src" -w /src $(IMAGE) 

clean:
	@docker rmi $(IMAGE)
