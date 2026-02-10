.model tiny
.code 
org 100h
Start:			mov ax, 0b800h
			mov es, ax
		
			mov bx, (80d*2*10+ 60d*2)
			mov byte ptr es: [bx], 02h
			mov es:[bx+1], 0001b
			mov es:[bx+1], (0001b or 80h)

			mov ax, 4c00h
			int 21h
end Start