.model tiny
.code
org 100h
Start:              mov ax, 3509h
                    int 21h

                    mov old09ofs, bx
                    mov bx, es
                    mov old09seg, bx


                    push 0
                    pop es

                    mov bx, 4*09h
                    cli
                    mov es:[bx], offset New09
                    mov ax, cs
                    mov es:[bx+2], ax
                    sti

                    pushf
                    push cs
                    call New09

                    mov ax, 3100h
                    mov dx, offset EOPPP
                    shr dx, 4
                    inc dx
                    int 21h

New09               proc
                    push ax bx es

                    push 0b800h
                    pop es
                    mov bx, (80d*5 + 40d)*2
                    mov ah, 4eh

                    in al, 60h
                    mov es:[bx], ax

                    in al, 61h
                    or al, 80h
                    out 61h, al
                    and al, not 80h
                    out 61h, al

                    mov al, 20h
                    out 20h, al

                    pop es bx ax

                    db 0eah
old09ofs            dw 0
old09seg            dw 0

New09               endp

EOPPP:
end Start
