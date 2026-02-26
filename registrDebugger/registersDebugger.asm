.286
.model tiny
.code
org 100h
locals @@

Start:
                call replace09Int
                call replace08Int

                pushf
                push cs
                call registersDebugger09Int

                call endResidentProgram


endResidentProgram          proc

                            mov ax, 3100h
                            mov dx, offset endOfProgram
                            shr dx, 4
                            inc dx
                            int 21h

endResidentProgram          endp
;------------------------------------------------------------------------------------------------
;Ends resident program.
;Entry:
;Exit:
;Expected: The label "endOfProgram" is located at the end of the code.
;Destroyed: ax, dx
;------------------------------------------------------------------------------------------------

registersDebugger09Int      proc
                            push ax bx es

                            in al, 60h
                            cmp al, 'W'
                            jne @@normalMode
                            mov cs:printfRegsFlag, 1h

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
;------------------------------------------------------------------------------------------------
;New 09 interrupt, which raises the printfRegsFlag to 1, when a key F11 is pressed.
;Entry:
;Exit:
;Expected:Old 09 interrupt was replaced to this function,
;old09ofs contains it's old segment and old09ofs contains it's old offset.
;Destroyed:
;------------------------------------------------------------------------------------------------


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
                            mov es:[bx], offset registersDebugger09Int
                            mov ax, cs
                            mov es:[bx+2], ax
                            sti

                            ret
replace09Int                endp
;------------------------------------------------------------------------------------------------
;Replaces interrupt 9 with a function registersDebugger09Int.
;Entry:
;Exit:
;Expected:
;Destroyed: ax, bx, es
;------------------------------------------------------------------------------------------------

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

                            ret
replace08Int                endp
;------------------------------------------------------------------------------------------------
;Replaces interrupt 8 with a function printfRegs08Int.
;Entry:
;Exit:
;Expected:
;Destroyed: ax, bx, es
;------------------------------------------------------------------------------------------------


printfRegs08Int             proc
                            cmp cs:printfRegsFlag, 0h
                            jne @@continue
                            jmp @@end

@@continue:                 push ax bx cx dx si di bp ds es ss
                            mov bp, sp

                            push 0b800h
                            pop es
                            mov bx, (80d*6+33d)*2

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

                            mov dh, 13d
                            mov dl, 14d
                            mov al, 30h

                            call printfRegistersFrame

                            mov al, 20h
                            out 20h, al

                            pop ss es ds bp di si dx cx bx ax

@@end:                      db 0eah
old08ofs                    dw 0
old08seg                    dw 0

printfRegs08Int             endp
;------------------------------------------------------------------------------------------------
;New 08 interrupt, which prints the updated values ​​of the processor registers
;and flags in a frame at each tick of the system timer.
;Entry:
;Exit:
;Expected:Old 08 interrupt was replaced to this function,
;old08ofs contains it's old segment and old08ofs contains it's old offset.
;Destroyed:
;------------------------------------------------------------------------------------------------


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

                        mov di, bx
                        push ax bx
                        call printfFlagsColumn
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
;------------------------------------------------------------------------------------------------
;Writes the value of the flags to the video memory
;Entry: ax = register of flags
;       bx = video memory offset of the CF
;Exit:
;Expected: es contains video memory segment.
;Destroyed: bx, cx, dx
;------------------------------------------------------------------------------------------------

printfFlagsColumn       proc

                        mov byte ptr es:[di], 'c'
                        mov byte ptr es:[di + 80d*2], 'z'
                        mov byte ptr es:[di + 80d*2*2], 's'
                        mov byte ptr es:[di + 80d*2*3], 'o'
                        mov byte ptr es:[di + 80d*2*4], 'p'
                        mov byte ptr es:[di + 80d*2*5], 'a'
                        mov byte ptr es:[di + 80d*2*6], 'i'
                        mov byte ptr es:[di + 80d*2*7], 'd'

                        add di, 2
                        mov al, '='
                        xor ah, ah
                        mov cx, 8h
                        push di
                        call printfVerticalString
                        mov al, 0
                        mov cx, 6h
                        call printfVerticalString
                        pop di


                        add di, 2
                        mov al, '0'
                        xor ah, ah
                        mov cx, 8h
                        call printfVerticalString

                        push di
                        mov al, 0
                        mov cx, 6h
                        call printfVerticalString
                        pop di

                        sub di, 4d
                        mov cx, 6h
                        call printfVerticalString

                        ret
printfFlagsColumn       endp
;----------------------------------------------------------------------------------------------
;Prints a column with processor flags with zeroed values.
;Entry: di = video memory offset.
;Exit:
;Expected: es contains the address of the video memory segment.
;Destroyed: ax, bx, cx, di
;----------------------------------------------------------------------------------------------

printfVerticalString     proc
                        mov bx, 80d*2
                        cmp ah, 0
                        je @@nextElem
                        neg bx

@@nextElem:             mov byte ptr es:[di], al
                        add di, bx
                        loop @@nextElem

                        ret

printfVerticalString     endp
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


printfFrameBackground           proc
                                push ax
                                call getFrameVideoMemoryOffset
                                pop ax

                                xor bx, bx
                                mov bl, dh
                                add bl, 6

printfFrameStr:                 xor cx, cx
                                mov cl, dl
                                add cl, 6

                                call printfFrameBackgroundString

                                add di, 160d

                                push ax
                                xor ax, ax
                                mov al, dl
                                cbw
                                shl ax, 1
                                sub di, ax
                                sub di, 12
                                pop ax

                                dec bl
                                cmp bl, 0
                                jne printfFrameStr

                                ret
printfFrameBackground           endp
;----------------------------------------------------------------------------------------------
;Prints frame centered frame background depending
;on the number of lines and their length in the message
;Entry: dh = the number of lines in the message
;       dl = max length of the string in the message
;       al = frame color binary code
;Exit:
;Expected:
;Destroyed: bx, di, cl
;----------------------------------------------------------------------------------------------


getFrameVideoMemoryOffset       proc

                                mov di, 25
                                mov al, dh
                                cbw
                                sub di, ax
                                shr di, 1
                                sub di, 3d
                                mov ax, 160
                                push dx
                                mul di
                                pop dx
                                mov di, ax

                                xor ax, ax
                                mov al, dl
                                cbw

                                mov bx, 80
                                sub bx, ax
                                sub bx, 6d
                                shr bx, 1
                                shl bx, 1
                                add di, bx

                                ret
getFrameVideoMemoryOffset       endp
;----------------------------------------------------------------------------------------------
;Gets a video memory offset for frame background depending
;on the number of lines and their length in the message
;Entry: dh = the number of lines in the message
;       dl = max length of the string in the message
;Exit:  di = video memory offset for frame background
;Expected:
;Destroyed: ax, bx, di
;----------------------------------------------------------------------------------------------


printfFrameBackgroundString         proc

@@printfByte:                       mov byte ptr es:[di+1], al
                                    add di, 2
                                    loop @@printfByte

                                    ret
printfFrameBackgroundString         endp
;----------------------------------------------------------------------------------------------
;Prints frameBackground string a certain of color
;Entry: di = starting address of the string
;       al = frame color binary code
;       cl = length of the string
;Exit:
;Expected: es contains the address of the video memory segment.
;Destroyed: di, cl
;----------------------------------------------------------------------------------------------

printfFrameForeground           proc

                                sub di, 160*2
                                add di, 2

                                mov al, 0C8h
                                mov byte ptr es:[di], al

                                add di, 2

                                xor cx, cx
                                mov cl, dl
                                add cl, 2

                                mov al, 0CDh
                                push ax
                                mov ah, 0
                                call printHorizontalString
                                pop ax

                                mov al, 0BCh
                                mov byte ptr es:[di], al

                                mov al, dh
                                add al, 3
                                push ax
                                cbw
                                mov bx, 160d
                                push dx
                                mul bx
                                pop dx
                                sub di, ax
                                pop ax

                                mov al, 0BBh
                                mov byte ptr es:[di], al

                                sub di, 2

                                xor cx, cx
                                mov cl, dl
                                add cl, 2
                                mov al, 0CDh
                                push ax
                                mov ah, 1
                                call printHorizontalString
                                pop ax

                                mov al, 0C9h
                                mov byte ptr es:[di], al
                                add di, 80d*2

                                mov cl, dh
                                add cl, 2
                                mov al, 0BAh
                                push ax
                                mov ah, 0
                                call printfVerticalString
                                pop ax

                                sub di, 80d*2

                                push ax
                                mov al, dl
                                add al, 3
                                cbw
                                shl ax, 1
                                add di, ax
                                pop ax

                                mov cl, dh
                                add cl, 2
                                mov al, 0BAh
                                push ax
                                mov ah, 1
                                call printfVerticalString
                                pop ax

                                add di, 2*80d
                                mov al, 0h
                                call printfInternalFrame

                                add dh, 4h
                                add dl, 4h
                                sub di, (80d*2)
                                add di, 6h
                                call printfInternalFrame

                                ret
printfFrameForeground           endp
;----------------------------------------------------------------------------------------------
;Prints centered frame foreground depending
;on the number of lines and their length in the message
;Entry: dh = the number of lines in the message
;       dl = max length of the string in the message
;       di = video memory offset of the lower right corner of the frame
;Exit:
;Expected: es contains the address of the video memory segment.
;Destroyed: ax, bx, cx, si, di
;----------------------------------------------------------------------------------------------


printHorizontalString   proc
                        mov bx, 2
                        cmp ah, 0
                        je @@nextElem
                        neg bx

@@nextElem:             mov byte ptr es:[di], al
                        add di, bx
                        loop @@nextElem

                        ret

printHorizontalString   endp
;----------------------------------------------------------------------------------------------
;Prints horizontal string
;Entry: al = symbol binary code
;       ah = direction flag (a = 0, di += 2; a != 0, di -= 2)
;       di = video memory offset
;       cx = length of string
;Exit:
;Expected: es contains the address of the video memory segment.
;Destroyed: cx, bx
;----------------------------------------------------------------------------------------------

printfInternalFrame             proc

                                sub di, 2
                                mov cl, dl
                                add cl, 2
                                mov ah, 1
                                call printHorizontalString

                                add di, 2
                                mov cl, dh
                                add cl, 2
                                mov ah, 0
                                call printfVerticalString

                                sub di, 2*80d
                                mov cl, dl
                                add cl, 2
                                mov ah, 0
                                call printHorizontalString

                                sub di, 2
                                mov cl, dh
                                add cl, 2
                                mov ah, 1
                                call printfVerticalString

                                ret
printfInternalFrame             endp
;----------------------------------------------------------------------------------------------
;Prints centered internal frame foreground depending
;on the number of lines and their length in the message
;Entry: al = internal frame symbol ascii-code
;       dh = the number of lines in the message
;       dl = max length of the string in the message
;       di = video memory offset of the upper right corner of the frame
;Exit:
;Expected: es contains the address of the video memory segment.
;Destroyed: ax, cx, si, di
;----------------------------------------------------------------------------------------------

printfRegistersFrame    proc

                        call printfFrameBackground

                        call printfFrameForeground

                        mov di, (80d*4+42d)*2
                        mov byte ptr es:[di], 0CBh
                        add di, 80d*2
                        mov al, 0BAh
                        xor ah, ah
                        mov cx, 15d
                        call printfVerticalString
                        mov byte ptr es:[di], 0CAh

                        sub di, 162d
                        xor al, al
                        mov ah, 1
                        mov cx, 15d
                        call printfVerticalString

                        add di, 164d
                        xor ah, ah
                        mov cx, 15d
                        call printfVerticalString

                        mov di, (80d*5+40d)*2
                        mov cx, 15d
                        call printfVerticalString

                        mov di, (80d*5+35d)*2
                        mov cx, 15d
                        call printfVerticalString

                        ret
printfRegistersFrame    endp
;----------------------------------------------------------------------------------------------
;Prints centered frame for register debugger.
;on the number of lines and their length in the message
;Entry: al = internal frame symbol ascii-code
;       dh = the number of frame line
;       dl = max length of the string in line
;Exit:
;Expected: es contains the address of the video memory segment.
;Destroyed: ax, bx, cx, dx, si, di
;----------------------------------------------------------------------------------------------


printfRegsFlag          db  0

endOfProgram:
end                     Start




