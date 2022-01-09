FROM alpine:latest
LABEL description="Advent Of Code 2016 - day 02"
RUN apk --update add \
	binutils \
	nasm \
	&& rm -rf /var/cache/apk/*
WORKDIR /src
COPY input *.sh *.asm /src/
RUN nasm -felf64 -o part1.o part1.asm \
	&& ld -m elf_x86_64 -o part1 part1.o
RUN nasm -felf64 -o part2.o part2.asm \
	&& ld -m elf_x86_64 -o part2 part2.o
CMD ./part1.sh && ./part2.sh
