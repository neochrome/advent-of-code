FROM alpine:latest
LABEL description="Advent Of Code 2016 - day 01"
RUN apk --update add \
	gawk \
	&& rm -rf /var/cache/apk/*
WORKDIR /src
COPY input part* /src/
CMD ./part1.sh && ./part2.sh
