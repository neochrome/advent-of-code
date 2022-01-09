package main

import (
	"crypto/md5"
	"encoding/hex"
	"fmt"
	"io"
)

type hasher func(string) string

func hash(str string) string {
	h := md5.New()
	io.WriteString(h, str)
	dig := h.Sum(nil)
	return hex.EncodeToString(dig)
}

func hash_rounds(n int) hasher {
	return func(str string) string {
		h := hash(str)
		for r := 0; r < n; r++ {
			h = hash(h)
		}
		return h
	}
}

func first_3(str string) byte {
	for c := 0; c < len(str)-2; c++ {
		if str[c] == str[c+1] && str[c+1] == str[c+2] {
			return str[c]
		}
	}
	return 0
}

func has_5_of(str string, ch byte) bool {
	for c := 0; c < len(str)-4; c++ {
		if str[c] == ch && str[c+1] == ch && str[c+2] == ch && str[c+3] == ch && str[c+4] == ch {
			return true
		}
	}
	return false
}

func last_key_index(salt string, hash hasher) int {
	hash_for := func(i int) string {
		return hash(fmt.Sprintf("%s%d", salt, i))
	}
	const SIZE = 1001
	var hashes [SIZE]string
	// pre-fill to enable look-ahead
	i := 0
	for ; i < SIZE; i++ {
		hashes[i] = hash_for(i)
	}
	keys_found := 0
	j := 0
	for {
		h := j % SIZE
		if ch := first_3(hashes[h]); ch != 0 {
			// look-ahead
			for k := 1; k < SIZE; k++ {
				l := (h + k) % SIZE
				if has_5_of(hashes[l], ch) {
					keys_found++
					if keys_found == 64 {
						return j
					}
					break
				}
			}
		}
		hashes[h] = hash_for(i)
		i++
		j++
	}
}

func main() {
	fmt.Printf("test: %d\n", last_key_index("abc", hash))
	fmt.Printf("part1: %d\n", last_key_index("jlmsuwbz", hash))
	fmt.Printf("part2: %d\n", last_key_index("jlmsuwbz", hash_rounds(2016)))
}
