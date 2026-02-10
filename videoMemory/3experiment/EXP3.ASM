.model tiny
.code

org 100h
Start:			mov ax, 0b800h
			mov es, ax
			xor bx, bx

			mov cx, 26d

loop_start:	
			mov byte ptr es:[bx], 65d
			mov es:[bx+1],(0001b or 80h)
			add bx, 80d*2
			loop loop_start

			mov ax, 4c00h
			int 21h
end Start