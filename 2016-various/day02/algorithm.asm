global _start

section .text
_start:
	mov dl, 5 ; start at key #5

next_char:
	call read_char
	cmp  eax, 0 ; EOF - we're done
	je   done

	mov ecx, direction_len ; directional offset
check_direction:
	cmp bl, byte [direction + ecx - 1]
	je  move
	loop check_direction
	; check newline and output digit if so
	cmp bl, `\n`
	jne exit_error
	mov bl, [digits + edx - 1]
	call write
	jmp next_char

move:
	; get new key by looking up current key
	; (4 bytes per key are used for lookup direction)
	; adjusted for directional offset and then
	; converted for zero-based offset
	mov dl, [keypad + (edx-1)*4 + ecx - 1]
	jmp next_char

done:
	mov bl, `\n`
	call write
	mov eax, 60
	xor edi, edi ; exit code 0
	syscall

exit_error:
	mov eax, 60
	mov edi, 1  ; exit code 1
	syscall

read_char:
	push rdx
	push rdi
	push rsi
	mov rax, 0      ; read
	mov rdx, 1      ; one byte
	mov rsi, buffer ; into buffer
	mov rdi, 0      ; from stdin
	syscall
	mov bl, [buffer]
	pop rsi
	pop rdi
	pop rdx
	ret

write:
	push rdx
	push rdi
	push rsi
	mov [buffer], bl ; prepare buffer
	mov rax, 1       ; write
	mov rdx, 1       ; one byte
	mov rsi, buffer  ; from buffer
	mov rdi, 1       ; to stdout
	syscall
	pop rsi
	pop rdi
	pop rdx
	ret

section .data
buffer        : db 0
digits        : db '123456789ABCD'
direction     : db 'UDLR' ; same order as offsets on keypad
direction_len : equ $-direction
