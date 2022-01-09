#!/usr/bin/env bash
let count=0
while read a1 b1 c1; do
	read a2 b2 c2
	read a3 b3 c3
	(( a1+a2>a3 && a1+a3>a2 && a2+a3>a1 && count++ ))
	(( b1+b2>b3 && b1+b3>b2 && b2+b3>b1 && count++ ))
	(( c1+c2>c3 && c1+c3>c2 && c2+c3>c1 && count++ ))
done < ./input
printf "part2: found %d possible triangles\n" $count
