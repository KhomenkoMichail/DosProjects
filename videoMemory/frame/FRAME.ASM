.model tiny
.code

locals @@
org 100h

Start:

main:           call initRegs
                cld

                call printfMessage
                call printfFrame

                mov ax, 4c00h
                int 21h



hashSymbolStrlen        proc

                        xor dl, dl
continue:               lodsb


                        cmp al, '#'
                        je endOfString
                        cmp al, 0Dh
                        je  endOfString

                        inc dl
                        jmp continue

endOfString:            ret
hashSymbolStrlen        endp
;----------------------------------------------------------------------------------------------
; Counts the number of characters from the starting address to the first '#' character
;Entry: si = string start address
;Exit:  dl = the number of characters from the starting address to the first '#' character
;Expected: direction flag = 0
;Destroyed: al, si, dl
;----------------------------------------------------------------------------------------------


getMessageParams      proc

                      ;mov cl, al

                      xor bx, bx

@@getNumOfStrings:    call hashSymbolStrlen
                      sub cl, dl

                      cmp cl, 0
                      je @@countIsZero
                      dec cl
@@countIsZero:        inc dh

                      cmp bl, dl
                      ja  @@shortString
                      mov bl, dl
@@shortString:        xor dl, dl
                      cmp cx, 0
@@notNewString:       jne @@getNumOfStrings

                    ;inc dh

                    cmp dl, bl
                    ja alreadyMaxLen
                    mov dl, bl

alreadyMaxLen:      ret
getMessageParams    endp
;----------------------------------------------------------------------------------------------
; Gets the number of strings in a command line message and the maximum length of those strings.
;Entry: si = command line message start address
;       cl = command line message length
;Exit:  dh = the number of strings in a command line
;       dl = the maximum length of command line message strings
;Expected:
;Destroyed: si, al, cx, bx, dx
;----------------------------------------------------------------------------------------------

getVideoMemoryVerticalOffset        proc
                                    xor ax, ax

                                    mov bx, 25
                                    mov al, dh
                                    cbw
                                    sub bx, ax
                                    shr bx, 1
                                    mov ax, 160
                                    push dx
                                    mul bx
                                    pop dx
                                    mov bx, ax

                                    ret
getVideoMemoryVerticalOffset        endp
;----------------------------------------------------------------------------------------------
;Gets video memory vertical position offset depending on the number of lines
;in the command line message.
;Entry: dh = the number of lines in the command line message.
;Exit:  bx = video memory vertical position offset
;Expected:
;Destroyed: ax, bx
;----------------------------------------------------------------------------------------------

printfCentredString         proc

                            push bp
                            mov bp, sp

                            mov si, [bp+4]
                            mov bx, [bp+6]

                            xor di, di

                            push dx
                            call hashSymbolStrlen

                            mov al, dl
                            cbw
                            mov di, ax
                            pop dx

                            push cx
                            mov cl, al

                            sub si, di
                            dec si

                            mov ax, 80
                            sub ax, di
                            shr ax, 1
                            shl ax, 1
                            add bx, ax

                            shl di, 1
                            add di, ax

                            push si
                            push cx
                            push bx
                            call myPuts
                            pop cx

                            sub bx, di
                            xor di, di

                            mov ax, si
                            mov sp, bp
                            pop bp
                            ret
printfCentredString         endp
;----------------------------------------------------------------------------------------------
;Prints a string in the center of video memory line
;Entry: [bp+4] = start address of the string
;       [bp+6] = video memory position offset
;Exit:  ax = end address of the string
;Expected: direction flag = 0, bx must be a multiple of 80d*2
;Destroyed: di, ax, si
;----------------------------------------------------------------------------------------------


myPuts              proc
                    push bp
                    mov bp, sp

                    mov si, [bp+8]
                    mov cx, [bp+6]
                    mov bx, [bp+4]

@@next:             lodsb
                    mov es:[bx], al
                    add bx, 2
                    loop @@next

                    mov sp, bp
                    pop bp
                    ret 6
myPuts              endp
;----------------------------------------------------------------------------------------------
;Prints a string of a given length
;Entry: [bp+8] = start address of the string
;       [bp+6] = length of the string
;       [bp+4] = video memory vertical position offset
;Exit
;Expected: direction flag = 0
;Destroyed: si, bx, al, cx
;----------------------------------------------------------------------------------------------

putCommandLineMessage           proc
                                call getVideoMemoryVerticalOffset

@@nextStr:                      push bx
                                push si

                                call printfCentredString
                                pop si
                                pop bx

                                mov si, ax
                                inc si

                                add bx, 80*2
                                loop @@nextStr

                                ret
putCommandLineMessage            endp
;----------------------------------------------------------------------------------------------
;Prints a centered command line message
;Entry: si = start address of the message
;       cx = number of message lines
;       bx = video memory vertical position offset
;Exit
;Expected: direction flag = 0, bx must be a multiple of 80d*2
;Destroyed: ax, bx, cx, si, di
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

printfFrameBackground            proc
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
printfFrameBackground            endp
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
;       ah = direction flag (a = 0, di += 2; a != 0, di -= 2)
;       di = video memory offset
;       cx = length of string
;Exit:
;Expected: es contains the address of the video memory segment.
;Destroyed: cx, bx
;----------------------------------------------------------------------------------------------

printfFrameForeground           proc
                                mov si, 82h
                                lodsb
                                mov ah, al

                                sub di, 160*2
                                ;mov al, dl
                                ;cbw
                                add di, 2

                                mov al, 0C8h
                                cmp ah, 30h
                                jne @@simpleChoice1
                                mov si, 8Ah
                                lodsb
@@simpleChoice1:                mov byte ptr es:[di], al

                                add di, 2

                                xor cx, cx
                                mov cl, dl
                                add cl, 2

                                mov al, 0CDh
                                cmp ah, 30h
                                jne @@simpleChoice2
                                mov si, 8Bh
                                lodsb

@@simpleChoice2:                push ax
                                mov ah, 0
                                call printHorizontalString
                                pop ax

                                mov al, 0BCh
                                cmp ah, 30h
                                jne @@simpleChoice3
                                mov si, 8Ch
                                lodsb
@@simpleChoice3:                mov byte ptr es:[di], al

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
                                cmp ah, 30h
                                jne @@simpleChoice4
                                mov si, 86h
                                lodsb
@@simpleChoice4:                mov byte ptr es:[di], al

                                sub di, 2

                                xor cx, cx
                                mov cl, dl
                                add cl, 2
                                mov al, 0CDh
                                cmp ah, 30h
                                jne @@simpleChoice5
                                mov si, 85h
                                lodsb

@@simpleChoice5:                push ax
                                mov ah, 1
                                call printHorizontalString
                                pop ax

                                mov al, 0C9h
                                cmp ah, 30h
                                jne @@simpleChoice6
                                mov si, 84h
                                lodsb
@@simpleChoice6:                mov byte ptr es:[di], al
                                add di, 80d*2

                                mov cl, dh
                                add cl, 2
                                mov al, 0BAh
                                cmp ah, 30h
                                jne @@simpleChoice7
                                mov si, 87h
                                lodsb

@@simpleChoice7:                push ax
                                mov ah, 0
                                call printVerticalString
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
                                cmp ah, 30h
                                jne @@simpleChoice8
                                mov si, 89h
                                lodsb

@@simpleChoice8:                push ax
                                mov ah, 1
                                call printVerticalString
                                pop ax

                                cmp ah, 30h
                                jne @@end
                                add di, 2*80d
                                call printInternalFrame

@@end:                          ret
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

initRegs                proc
                        mov ax, 0b800h
                        mov es, ax

                        xor ax, ax
                        xor bx, bx
                        xor cx, cx
                        xor dx, dx

                        ret
initRegs                endp
;----------------------------------------------------------------------------------------------
;Inits regs.
;Entry:
;Exit:
;Expected:
;Destroyed: ax, bx, cx, dx, es
;----------------------------------------------------------------------------------------------

printfMessage           proc

                        mov si, 80h
                        lodsb

                        mov cl, al

                        mov si, 82h
                        lodsb

                        inc si
                        sub cl, 3

                        sub al, 30h
                        cmp al, 0
                        jne @@simpleMode
                        add si, 13d             ;;
                        sub cl, 13d             ;;changes

@@simpleMode:           push si
                        call getMessageParams
                        pop si

                        mov cl, dh
                        xor ch, ch

                        call putCommandLineMessage
                        ret

printfMessage           endp
;----------------------------------------------------------------------------------------------
;Сalculates position and prints a centered command line message
;Entry:
;Exit:
;Expected: direction flag = 0, es contains the address of the video memory segment.
;Destroyed: ax, bx, cx, si
;----------------------------------------------------------------------------------------------

printInternalFrame              proc

                                mov si, 88h
                                lodsb

                                sub di, 2
                                mov cl, dl
                                add cl, 2
                                mov ah, 1
                                call printHorizontalString

                                add di, 2
                                mov cl, dh
                                add cl, 2
                                mov ah, 0
                                call printVerticalString

                                sub di, 2*80d
                                mov cl, dl
                                add cl, 2
                                mov ah, 0
                                call printHorizontalString

                                sub di, 2
                                mov cl, dh
                                add cl, 2
                                mov ah, 1
                                call printVerticalString

                                ret
printInternalFrame              endp
;----------------------------------------------------------------------------------------------
;Prints centered internal frame foreground depending
;on the number of lines and their length in the message
;Entry: dh = the number of lines in the message
;       dl = max length of the string in the message
;       di = video memory offset of the upper right corner of the frame
;Exit:
;Expected: es contains the address of the video memory segment.
;Destroyed: ax, cx, si, di
;----------------------------------------------------------------------------------------------

getFrameColor           proc
                        mov al, 01110000b
                        mov si, 82h
                        mov bl, [si]
                        cmp bl, 30h
                        jne @@end

                        mov si, 8Eh
                        lodsb
                        sub al, 30h
                        cmp al, 10d
                        jb @@less10_1
                        sub al, 7d
@@less10_1:             shl al, 4

                        mov bl, [si]
                        sub bl, 30h
                        cmp bl, 10d
                        jb @@less10_2
                        sub bl, 7d
@@less10_2:             add al, bl

@@end:                  ret
getFrameColor           endp
;----------------------------------------------------------------------------------------------
;Gets the color attribute of the frame depending on the selected mode
;Entry:
;Exit:  al = color attribute of the frame
;Expected:  [82h] = frame mode
;           [8Eh] and [8Fh] = hex color code
;Destroyed: al, bl, si
;----------------------------------------------------------------------------------------------

printfFrame             proc
                        call getFrameColor

                        call printfFrameBackground

                        call printfFrameForeground
                        ret

printfFrame             endp
;----------------------------------------------------------------------------------------------
;Prints a centered frame.
;Entry: dh = the number of lines in the message
;       dl = max length of the string in the message
;Exit:
;Expected: direction flag = 0, es contains the address of the video memory segment,
;          [82h] = frame mode.
;Destroyed: ax, bx, cx, si, di
;----------------------------------------------------------------------------------------------

end Start
