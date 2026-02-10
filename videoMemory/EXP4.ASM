.model tiny
.code
org 100h


Start:			mov ax, 0b800h
			mov es, ax
			mov bx, 80d*2*10+10*2
		
			mov si, offset Hello

			mov cx, 13d
			
loop_start:
			lodsb 
			mov es:[bx], al
			add bx, 2
			loop loop_start

			mov ax, 4c00h
			int 21h

Hello			db 'Hello world!$'
end Start 