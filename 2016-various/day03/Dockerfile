FROM alpine:latest
LABEL description="Advent Of Code 2016 - day 03"
RUN apk --update add \
	bash \
	&& rm -rf /var/cache/apk/*
WORKDIR /src
COPY input *.sh /src/
CMD ./part1.sh && ./part2.sh
