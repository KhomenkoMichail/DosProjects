.286
.model tiny
.code
org 100h
locals @@

Start:          mov ax, 3509h
                int 21h

                mov old09ofs, bx
                mov bx, es
                mov old09seg, bx

                push 0
                pop es

                mov bx, 4*09h
                cli
                mov es:[bx], offset registersDebugger09Int
                mov ax, cs
                mov es:[bx+2], ax
                sti

                pushf
                push cs
                call registersDebugger09Int

                mov ax, 3100h
                mov dx, offset endOfProgram
                shr dx, 4                               ; //FIXME
                inc dx                                  ;
                int 21h



registersDebugger09Int      proc
                            push ax bx es

                            in al, 60h
                            ;cmp al, 'W'                   ;; //FIXME
                            ;jne @@normalMode
                            ;cmp int08WasReplaced, 0h
                            ;jne @@normalMode

                            call replace08Int

@@normalMode:               in al, 61h
                            or al, 80h
                            out 61h, al
                            and al, not 80h
                            out 61h, al

                            mov al, 20h
                            out 20h, al

                            pop es bx ax

                            db 0eah
old09ofs                    dw 0
old09seg                    dw 0

registersDebugger09Int      endp

replace08Int                proc

                            mov ax, 3508h
                            int 21h

                            mov old08ofs, bx
                            mov bx, es
                            mov old08seg, bx

                            push 0
                            pop es

                            mov bx, 4*08h
                            cli
                            mov es:[bx], offset printfRegs08Int
                            mov ax, cs
                            mov es:[bx+2], ax
                            sti

                            mov int08WasReplaced, 1h

                            ret
replace08Int                endp


printfRegs08Int             proc
                            push ax bx cx dx si di bp ds es ss
                            mov bp, sp

                            push 0b800h
                            pop es
                            mov bx, (80d*5+33d)*2

                            mov byte ptr es:[bx], 'a'
                            mov byte ptr es:[bx+2], 'x'
                            call printfHexRegValue

                            add bx, 80d*2
                            mov byte ptr es:[bx], 'b'
                            mov byte ptr es:[bx+2], 'x'
                            mov ax, [bp+16]
                            call printfHexRegValue

                            add bx, 80d*2
                            mov byte ptr es:[bx], 'c'
                            mov byte ptr es:[bx+2], 'x'
                            mov ax, [bp+14]
                            call printfHexRegValue

                            add bx, 80d*2
                            mov byte ptr es:[bx], 'd'
                            mov byte ptr es:[bx+2], 'x'
                            mov ax, [bp+12]
                            call printfHexRegValue

                            add bx, 80d*2
                            mov byte ptr es:[bx], 's'
                            mov byte ptr es:[bx+2], 'i'
                            mov ax, [bp+10]
                            call printfHexRegValue

                            add bx, 80d*2
                            mov byte ptr es:[bx], 'd'
                            mov byte ptr es:[bx+2], 'i'
                            mov ax, [bp+8]
                            call printfHexRegValue

                            add bx, 80d*2
                            mov byte ptr es:[bx], 'b'
                            mov byte ptr es:[bx+2], 'p'
                            mov ax, [bp+6]
                            call printfHexRegValue

                            add bx, 80d*2
                            mov byte ptr es:[bx], 's'
                            mov byte ptr es:[bx+2], 'p'
                            mov ax, sp
                            sub ax, 13d*2
                            call printfHexRegValue

                            add bx, 80d*2
                            mov byte ptr es:[bx], 'd'
                            mov byte ptr es:[bx+2], 's'
                            mov ax, [bp+4]
                            call printfHexRegValue

                            add bx, 80d*2
                            mov byte ptr es:[bx], 'e'
                            mov byte ptr es:[bx+2], 's'
                            mov ax, [bp+2]
                            call printfHexRegValue

                            add bx, 80d*2
                            mov byte ptr es:[bx], 's'
                            mov byte ptr es:[bx+2], 's'
                            mov ax, [bp]
                            call printfHexRegValue

                            add bx, 80d*2
                            mov byte ptr es:[bx], 'c'
                            mov byte ptr es:[bx+2], 's'
                            mov ax, [bp+22]
                            call printfHexRegValue

                            add bx, 80d*2
                            mov byte ptr es:[bx], 'i'
                            mov byte ptr es:[bx+2], 'p'
                            mov ax, [bp+20]
                            call printfHexRegValue

                            mov ax, [bp+24]
                            sub bx, 80d*2*12d
                            add bx, 11d*2
                            call printfFlags

                            mov al, 20h
                            out 20h, al

                            pop ss es ds bp di si dx cx bx ax      ;;//FIXME

                            db 0eah
old08ofs                    dw 0
old08seg                    dw 0

printfRegs08Int             endp



printfHexRegValue           proc

                            xor dx, dx
                            mov dl, ah
                            shr dl, 4h

                            add dl, 30h
                            cmp dl, 39h
                            jbe @@notLetter1
                            add dl, 7h

@@notLetter1:               mov byte ptr es:[bx+6], dl

                            mov dl, ah
                            and dl, 00001111b
                            add dl, 30h
                            cmp dl, 39h
                            jbe @@notLetter2
                            add dl, 7h

@@notLetter2:               mov byte ptr es:[bx+8], dl

                            mov dl, al
                            shr dl, 4h

                            add dl, 30h
                            cmp dl, 39h
                            jbe @@notLetter3
                            add dl, 7h

@@notLetter3:               mov byte ptr es:[bx+10d], dl

                            mov dl, al
                            and dl, 00001111b
                            add dl, 30h
                            cmp dl, 39h
                            jbe @@notLetter4
                            add dl, 7h

@@notLetter4:               mov byte ptr es:[bx+12d], dl

                            ret
printfHexRegValue           endp
;------------------------------------------------------------------------------------------------
;Writes the hexadecimal value of the register to the video memory
;Entry: ax = current register value
;       bx = video memory offset of the beginning of the inscription reduced by 6 bytes
;       (the first 6 bytes are used to write the register name)
;Exit:
;Expected: es contains video memory segment
;Destroyed: dx
;------------------------------------------------------------------------------------------------

printfFlags             proc

                        mov byte ptr es:[bx], 'c'
                        mov byte ptr es:[bx + 80d*2], 'z'
                        mov byte ptr es:[bx + 80d*2*2], 's'
                        mov byte ptr es:[bx + 80d*2*3], 'o'
                        mov byte ptr es:[bx + 80d*2*4], 'p'
                        mov byte ptr es:[bx + 80d*2*5], 'a'
                        mov byte ptr es:[bx + 80d*2*6], 'i'
                        mov byte ptr es:[bx + 80d*2*7], 'd'

                        push ax bx
                        mov di, bx
                        add di, 2
                        mov al, '='
                        xor ah, ah
                        mov cx, 8h
                        call printVerticalString

                        pop bx ax
                        push ax bx
                        mov di, bx
                        add di, 4h
                        mov al, '0'
                        xor ah, ah
                        mov cx, 8h
                        call printVerticalString
                        pop bx ax

                        add bx, 4h

                        mov dx, ax
                        and dx, 0000000000000001b
                        cmp dx, 0
                        je @@zeroCF
                        mov byte ptr es:[bx], '1'
@@zeroCF:

                        mov dx, ax
                        and dx, 0000000001000000b
                        cmp dx, 0
                        je @@zeroZF
                        mov byte ptr es:[bx + 80d*2], '1'
@@zeroZF:
                        mov dx, ax
                        and dx, 0000000010000000b
                        cmp dx, 0
                        je @@zeroSF
                        mov byte ptr es:[bx + 80d*2*2], '1'
@@zeroSF:
                        mov dx, ax
                        and dx, 0000100000000000b
                        cmp dx, 0
                        je @@zeroOF
                        mov byte ptr es:[bx + 80d*2*3], '1'
@@zeroOF:
                        mov dx, ax
                        and dx, 0000000000000100b
                        cmp dx, 0
                        je @@zeroPF
                        mov byte ptr es:[bx + 80d*2*4], '1'
@@zeroPF:
                        mov dx, ax
                        and dx, 0000000000010000b
                        cmp dx, 0
                        je @@zeroAF
                        mov byte ptr es:[bx + 80d*2*5], '1'
@@zeroAF:
                        mov dx, ax
                        and dx, 0000001000000000b
                        cmp dx, 0
                        je @@zeroIF
                        mov byte ptr es:[bx + 80d*2*6], '1'
@@zeroIF:
                        mov dx, ax
                        and dx, 0000010000000000b
                        cmp dx, 0
                        je @@zeroDF
                        mov byte ptr es:[bx + 80d*2*7], '1'

@@zeroDF:
                        ret
printfFlags             endp


printVerticalString     proc
                        mov bx, 80d*2
                        cmp ah, 0
                        je @@nextElem
                        neg bx

@@nextElem:             mov byte ptr es:[di], al
                        add di, bx
                        loop @@nextElem

                        ret

printVerticalString     endp
;----------------------------------------------------------------------------------------------
;Prints vertical string
;Entry: al = symbol binary code
;       ah = direction flag (ah = 0, di += 80d*2; ah != 0, di -= 80d*2)
;       di = video memory offset
;       cx = length of string
;Exit:
;Expected: es contains the address of the video memory segment.
;Destroyed: cx, bx, di
;----------------------------------------------------------------------------------------------


int08WasReplaced  db  0


endOfProgram:
end                     Start




