#!/usr/bin/env bash
let count=0
while read a b c; do
	(( a+b>c && a+c>b && b+c>a && count++ ))
done < ./input
printf "part1: found %d possible triangles\n" $count
