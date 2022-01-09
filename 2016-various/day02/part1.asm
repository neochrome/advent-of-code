%include "./algorithm.asm"

keypad: ; offsets: UDLR
	db 1,4,1,2 ; #1
	db 2,5,1,3 ; #2
	db 3,6,2,3 ; #3
	db 1,7,4,5 ; #4
	db 2,8,4,6 ; #5
	db 3,9,5,6 ; #6
	db 4,7,7,8 ; #7
	db 5,8,7,9 ; #8
	db 6,9,8,9 ; #9
