.model tiny
.code

locals @@
org 100h

PASSWORD_SUCCESS            equ     1h
PASSWORD_LENGTH             equ     8h

MESSAGE_SUCCESS_LENGTH      equ     15d
MESSAGE_FAIL_LENGTH         equ     14d

MESSAGE_SUCCESS_COLOR       equ     0AEh
MESSAGE_FAIL_COLOR          equ     0CFh

ENTER_ASCII_CODE            equ     0dh

Start:

                            jmp main

password                    db      26h, 3Ah, 33h, 3Ch, 39h, 0Bh, 3Dh, 27h
requestMessage              db      "Please enter the password: ", 0Dh, 0Ah, '$'
messageSuccess              db      "Access granted!"
messageFail                 db      "Access denied."


bufferData                  db 256 dup(0)

main:
                            cld

                            mov ah, 09h
                            mov dx, offset requestMessage
                            int 21h

                            call getPassword

                            push cx
                            call decipherPassword
                            pop cx

                            push cx
                            call cmpPasswords

                            cmp dx, PASSWORD_SUCCESS
                            je grant
                            call    printfFailMessage

grant:                      call    printfSuccessMessage

endProgram:
                            mov ax, 4c00h
                            int 21h


getPassword                 proc

                            xor cx, cx
                            lea si, bufferData

@@nextChar:                 mov ah, 01h
                            int 21h
                            cmp al, ENTER_ASCII_CODE
                            je @@end

                            mov [si], al
                            inc si
                            inc cx
                            jmp @@nextChar

@@end:
                            ret
getPassword                 endp
;----------------------------------------------------------------------------------------------
;Gets a string from standard input and writes it to the input buffer.
;Entry:
;Exit: cx = length of input password
;Expected:
;Destroyed: ax, cx, si
;----------------------------------------------------------------------------------------------


getStringHash               proc

                            xor bh, bh

                            xor ch, ch
                            mov ax, 5381d

@@nextChar:                 mov bl, [si]
                            add ax, bx
                            shr ax, 5h
                            inc si
                            loop @@nextChar

                            ret
getStringHash               endp
;----------------------------------------------------------------------------------------------
;Calculates string hash.
;Entry: si = string start address
;       cl = string length
;Exit:  ax = string hash
;Expected: clean direction flag.
;Destroyed: ax, bx, dx, cx
;----------------------------------------------------------------------------------------------


cmpPasswords                proc
                            push bp
                            mov bp, sp

                            xor ax, ax
                            mov byte ptr al, [bp + 4]
                            add al, 1
                            sub sp, ax

                            lea si, bufferData
                            mov di, sp
                            mov cx, [bp+4]
                            rep movsb

                            mov cx, [bp+4]
                            mov si, sp
                            call getStringHash

                            mov bx, ax

                            mov cl, PASSWORD_LENGTH
                            lea si, password
                            push bx
                            call getStringHash
                            pop bx

                            xor dx, dx
                            cmp ax, bx
                            jne @@end

                            push cs
                            pop es

                            push cs
                            pop ds

                            lea si, password
                            mov di, sp

                            xor ch, ch
                            mov cx, [bp+4]
                            cmp cx, PASSWORD_LENGTH
                            jne @@end

                            repe cmpsb
                            jne @@end
                            inc dx

@@end:                      mov sp, bp
                            pop bp
                            ret 2
cmpPasswords                endp
;----------------------------------------------------------------------------------------------
;Compares correct password with password from input.
;Entry: [bp + 4] = length of input password.
;Exit: dx = correct password flag (== 1 if correct, else == 0)
;Expected: clean direction flag.
;Destroyed: ax, bx, cx, dx, si, di
;----------------------------------------------------------------------------------------------


printfFrame                 proc

                            call printfFrameBackground

                            call printfFrameForeground
                            ret

printfFrame                 endp
;----------------------------------------------------------------------------------------------
;Prints a centered frame.
;Entry: al = frame color binary code
;       dh = the number of lines in the message
;       dl = max length of the string in the message
;Exit:
;Expected: direction flag = 0, es contains the address of the video memory segment,
;          [82h] = frame mode.
;Destroyed: ax, bx, cx, si, di
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
;       ah = direction flag (ah = 0, di += 80d*2; ah != 0, di -= 80d*2)
;       di = video memory offset
;       cx = length of string
;Exit:
;Expected: es contains the address of the video memory segment.
;Destroyed: cx, bx
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
                                push ax
                                mov ah, 1
                                call printVerticalString
                                pop ax

                                mov al, 0h
                                add di, 80d*2
                                call printInternalFrame

                                push ax cx si di dx

                                add dh, 4h
                                add dl, 4h
                                sub di, (80d*2)
                                add di, 6h
                                call printInternalFrame

                                pop dx di si cx ax

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


printfFrameBackgroundString         proc

@@printfByte:                       inc di
                                    stosb
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


printInternalFrame              proc

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
;Entry: al = internal frame symbol ascii-code
;       dh = the number of lines in the message
;       dl = max length of the string in the message
;       di = video memory offset of the upper right corner of the frame
;Exit:
;Expected: es contains the address of the video memory segment.
;Destroyed: ax, cx, si, di
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


printfSuccessMessage            proc

                                mov ax, 0b800h
                                mov es, ax

                                mov al, MESSAGE_SUCCESS_COLOR
                                mov dh, 1
                                mov dl, MESSAGE_SUCCESS_LENGTH

                                call printfFrame

                                mov di, (12d * 80d + 32d) * 2

                                lea si, messageSuccess
                                mov cx, MESSAGE_SUCCESS_LENGTH
                                mov ah, MESSAGE_SUCCESS_COLOR

@@print_char:
                                lodsb
                                stosw
                                loop @@print_char


                                ret
printfSuccessMessage            endp
;----------------------------------------------------------------------------------------------
;Prints message "Access granted!" in a green centered frame.
;Entry:
;Exit:
;Expected: clean direction flag.
;Destroyed: ax, bx, cx, dx, si, di, es
;----------------------------------------------------------------------------------------------


printfFailMessage               proc

                                mov ax, 0b800h
                                mov es, ax

                                mov al, MESSAGE_FAIL_COLOR
                                mov dh, 1
                                mov dl, MESSAGE_FAIL_LENGTH

                                call printfFrame

                                mov di, (12d * 80d + 33d) * 2

                                lea si, messageFail
                                mov cx, MESSAGE_FAIL_LENGTH
                                mov ah, MESSAGE_FAIL_COLOR

@@print_char:
                                lodsb
                                stosw
                                loop @@print_char

                                jmp endProgram

                                ret
printfFailMessage               endp
;----------------------------------------------------------------------------------------------
;Prints message "Access denied!" in a red centered frame.
;Entry:
;Exit:
;Expected: clean direction flag.
;Destroyed: ax, bx, cx, dx, si, di, es
;----------------------------------------------------------------------------------------------

decipherPassword        proc
                        mov cx, PASSWORD_LENGTH

                        lea si, password

@@nextChar:             mov bl, 52h
                        mov byte ptr al, [si]
                        xor al, bl
                        mov byte ptr [si], al
                        inc si
                        loop @@nextChar

                        ret
decipherPassword        endp

end             Start
