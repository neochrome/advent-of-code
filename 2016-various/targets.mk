all: build run

build:
	@docker build -t $(IMAGE) .

run:
	@docker run --rm -it $(IMAGE)

clean:
	@docker rmi $(IMAGE)
