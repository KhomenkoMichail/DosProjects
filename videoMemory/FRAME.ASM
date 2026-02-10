.model tiny
.code
org 100h

Start:			mov ax, 0b800h
			mov es, ax
			xor ax, ax
			
			mov si, 80h
			lodsb

			mov cl, al
			
			shr ax, 1
			mov bx, 40d
			sub bx, ax
			shl bx, 1
			add bx, (13d*80d*2)
		
loop_start:
			lodsb
			mov es:[bx], al
			add bx, 2
			loop loop_start
			

			mov ax, 4c00h
			int 21h
end Start