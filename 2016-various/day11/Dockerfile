FROM alpine:latest
LABEL description="Advent Of Code 2016 - day 11"
RUN apk --update add \
	build-base \
	&& rm -rf /var/cache/apk/*
WORKDIR /src
CMD gcc -std=c99 -o /tmp/solution solution.c && /tmp/solution
