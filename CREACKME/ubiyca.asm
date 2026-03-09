.286
.model tiny
.code
org 100h
locals @@

Start:

            call replace09Int

            pushf
            push cs
            call ubiycaCoda09Int

            mov ax, 3100h
            mov dx, offset endOfProgram
            shr dx, 4
            inc dx
            int 21h



replace09Int                proc
                            mov ax, 3509h
                            int 21h

                            mov old09ofs, bx
                            mov bx, es
                            mov old09seg, bx

                            push 0
                            pop es

                            mov bx, 4*09h
                            cli
                            mov es:[bx], offset ubiycaCoda09Int
                            mov ax, cs
                            mov es:[bx+2], ax
                            sti

                            ret
replace09Int                endp


ubiycaCoda09Int             proc
                            push bp
                            mov bp, sp
                            push ax bx di es

                            in al, 60h
                            cmp al, 'W'
                            je @@modeON

@@normalMode:               in al, 61h
                            or al, 80h
                            out 61h, al
                            and al, not 80h
                            out 61h, al

                            mov al, 20h
                            out 20h, al

                            pop es di bx ax
                            pop bp

                            db 0eah
old09ofs                    dw 0
old09seg                    dw 0

@@modeON:                   mov si, 0127h
                            ;mov ax, [bp+2]
                            ;mov cs, ax
                            pop es di bx ax
                            pop bp
                            jmp cs:[si]


ubiycaCoda09Int             endp
endOfProgram:
end                       Start
