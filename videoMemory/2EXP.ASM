.model tiny
.code 
org 100h
Start:			mov ax, 0b800h
			mov es, ax

			mov di, (80d*2*13)
			mov cx, 80d

			mov ax, 110111111110011b
			rep stosw


			mov ax, 4c00h
			int 21h
end Start