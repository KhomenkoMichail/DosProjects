.model tiny
.code
org 100h

Start:              mov ax, 0b800h
                    mov es, ax
                    xor ax, ax

                    mov si, 80h
                    lodsb

                    mov cl, al
                    xor ch, ch

                    xor dl, dl
                    xor dh, dh
                    xor bl, bl

                    dec dl
getNumOfStrings:    inc dl
                    lodsb
                    cmp al, '#'
                    jne notNewString
                    dec dl
                    inc dh
                    cmp bl, dl
                    ja  shortString
                    mov bl, dl
shortString:        xor dl, dl
notNewString:       loop getNumOfStrings

                    inc dh

                    cmp dl, bl
                    ja alreadyMaxLen
                    mov dl, bl
alreadyMaxLen:
                    mov cl, dh
                    xor ch, ch
                    mov si, 81h

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

                    xor di, di
                    dec di

printfCurString:    lodsb
                    cmp al, '#'
                    je  isEndOfString
                    cmp al, 0Dh
                    je  isEndOfString
                    inc di
                    jmp printfCurString

isEndOfString:      sub si, di
                    dec si

                    mov ax, 80
                    sub ax, di
                    shr ax, 1
                    shl ax, 1
                    add bx, ax

                    shl di, 1
                    add di, ax

printfString:       lodsb
                    cmp al, '#'
                    je  conditionCheck
                    cmp al, 0Dh
                    je  conditionCheck
                    mov es:[bx], al
                    add bx, 2
                    jmp printfString

conditionCheck:     add bx, 80*2
                    sub bx, di

                    xor di, di
                    loop printfCurString



            ; printing frame:

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

                xor bx, bx
                mov bl, dh
                add bl, 6

printfFrameStr:         xor cx, cx
                        mov cl, dl
                        add cl, 6



printfFrameByte:        mov byte ptr es:[di+1], 01110000b
                        add di, 2
                        loop printfFrameByte

                        add di, 160d

                        xor ax, ax
                        mov al, dl
                        cbw
                        shl ax, 1
                        sub di, ax
                        sub di, 12



                        dec bl
                        cmp bl, 0
                        jne printfFrameStr


                sub di, 160*2
                mov al, dl
                cbw
                add di, 2
                mov byte ptr es:[di], 0C8h

                add di, 2

                xor cx, cx
                mov cl, dl
                add cl, 2

printHorizon1:  mov byte ptr es:[di], 0CDh
                add di, 2
                loop printHorizon1

                mov byte ptr es:[di], 0BCh

                mov al, dh
                add al, 3
                cbw
                mov bx, 160d
                push dx
                mul bx
                pop dx
                sub di, ax
                mov byte ptr es:[di], 0BBh
                sub di, 2

                xor cx, cx
                mov cl, dl
                add cl, 2

printHorizon2:  mov byte ptr es:[di], 0CDh
                sub di, 2
                loop printHorizon2

                mov byte ptr es:[di], 0C9h
                add di, 80d*2

                mov cl, dh
                add cl, 2

printfVert1:	mov byte ptr es:[di], 0BAh
		add di, 80d*2
		loop printfVert1

                mov cl, dh
                add cl, 2

                sub di, 80d*2

                mov al, dl
                add al, 3
                cbw
                shl ax, 1
                add di, ax

printfVert2:	mov byte ptr es:[di], 0BAh
		sub di, 80d*2
		loop printfVert2

                mov ax, 4c00h
                int 21h
end Start
