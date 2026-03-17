# Educational task "CRACKME"

[Русскоязычная версия](readmeRUS.md)

## Description
The program asks for a password. If the user enters the correct password, the phrase "access granted!" appears; otherwise the phrase "access denied." is displayed.

The program contains two specially created vulnerabilities that cause the program to display the "access granted!" phrase when an incorrect password is entered.

The educational task was to create such a program and to find vulnerabilities in your partner's program.

## My program

You can see the code of my program in the file "creackme.asm".

It displays a red frame with a message when an incorrect password is entered, and a green frame otherwise.


<img width="1023" height="649" alt="image" src="https://github.com/user-attachments/assets/215533fd-5f20-43f7-87be-ae2c5cd71cef" />


<img width="1023" height="640" alt="image" src="https://github.com/user-attachments/assets/bb01b7be-2af3-4e51-b3f1-d1c1814e75f2" />


### Program vulnerabilities:
#### 1. Buffer overflow
The password entered by the user is processed using 01 function of the 21 interrupt and is written into the buffer located before the program code.

The program doesn't check the length of the entered password. Therefore, the entered password could exceed the buffer and alter the program code, for example, replacing the password check with "NOP."

```
bufferData                  db 256 dup(0)

main:
                            cld

                            mov ah, 09h
                            mov dx, offset requestMessage
                            int 21h

                            call getPassword

                            push cx
                            call cmpPasswords

                            cmp dx, PASSWORD_SUCCESS
                            je grant
                            call    printfFailMessage

grant:                      call    printfSuccessMessage

endProgram:
                            mov ax, 4c00h
                            int 21h
```

#### 2. 8-bit register integer overflow

The function "cmpPasswords" uses stack-based argument passing in the Pascal declaration. The only argument it takes is the length of the string entered by the user. Then it allocates an array on the stack large enough to save the user's password.

The password length is placed in the al register, which is incremented and subtracted from the sp register. The new sp value is used as the address to copy the user password from the buffer.

When entering a 255-character password, the al register overflows and no memory is allocated on the stack, while 255 bytes are copied from the password buffer using the sp pointer, which can change the function return address.

```
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
```
Input file that hacks the program:


<img width="1032" height="492" alt="image" src="https://github.com/user-attachments/assets/3d14905d-cb0e-4490-88d1-0f7e1b01afef" />


The 02 and 03 bytes contain the address of the function "printfSuccessMessage" (little-endian). 0FFh byte - ASCII code of [Enter].

## Hacking a partner's program
I received the file "krakra.com" from my colleague. I used IDA disassembler and here are my steps to crack his program:

#### 1. Quick look

By looking through the program code in IDA's text mode, I noticed data buffer embedded within the code. This reminded me of my buffer overflow vulnerability. Then I started looking for a function that handles user input. This was the first function in the program. It does not check the length of the entered password and writes it to the address of this buffer.


<img width="1695" height="1054" alt="image" src="https://github.com/user-attachments/assets/66642605-1d66-4d96-9386-cebe94377684" />


To exploit this vulnerability, I created a file "vzlom2" containing 53 junk bytes (the difference between 018ch (the address of the function following the buffer) and 0157h (the address of the buffer's beginning)). I followed these by the "EB" byte (the command "JMP" byte code), and the "94" byte (the difference between the address of the output "access approved" phrase and the current IP address). I ended the file with the "0D" byte (the [Enter] ASCII code).


<img width="1021" height="642" alt="image" src="https://github.com/user-attachments/assets/c49f9dd4-e5cd-49b5-8229-f96d2cee2fcf" />


Using input redirection I got a success message:


<img width="745" height="90" alt="image" src="https://github.com/user-attachments/assets/386af1fe-017c-4492-9f9b-17e64e1800c3" />


#### 2. Finding a difficult vulnerability

To find a difficult vulnerability, I began to look through each function of the disassembled code. After the getting password function, the program contains two strange functions that, according to some incomprehensible logic, change the register values. At first I didn't understand what they were doing and continued viewing disassembled code.


<img width="1455" height="1310" alt="image" src="https://github.com/user-attachments/assets/53f2f40c-18fb-4f1e-8674-4d2698e4d913" />


Next there are two calls to the function for obtaining password hashes (user and correct).


<img width="1312" height="773" alt="image" src="https://github.com/user-attachments/assets/3eeedd60-5e51-4dd9-ad08-848c4498997a" />


The address of the beginning of the string containing the password is getting by the function through the di register. When receiving the user password hash, the buffer address is moved to di explicitly, but before processing the correct password, a strange function follows.


<img width="1449" height="327" alt="image" src="https://github.com/user-attachments/assets/f671fb8b-7ea6-4d97-8ed2-affdf8d37f44" />


After reviewing the second function of the program several times, I noticed that the registers it modifies are not used anywhere else, and its only result is moving the value stored at the address contained in si to di. After this function call di must contain the address of the correct password to pass it to the function that calculates its hash. This means that the address of the correct password is located at address 017Ch, which is located immediately after the buffer with the incoming password.

To exploit this vulnerability, I created a file "vzlom1" containing 37 junk bytes (the difference between 017Ch (the address which contains the address of correct password) and 0157h (the address of the buffer's beginning)). Next I wrote two bytes of the incoming password buffer address: "57" and "01" (little-endian). I ended the file with the "0D" byte (the [Enter] ASCII code).


<img width="1016" height="645" alt="image" src="https://github.com/user-attachments/assets/afc1f257-3e39-4833-9e47-06c585c8d494" />


With this input, the program calculates the hash of the user's password twice and compares it with itself. The check always passes and the program displays
"access approved" :


<img width="750" height="88" alt="image" src="https://github.com/user-attachments/assets/00358047-cff4-4f64-9515-c2349f7295b0" />


## Patcher in C language.

### Description

To make hacking more effective, I wrote patcher program in C (the source files are located in the patch folder). The program accepts two arguments: the filename of the program being hacked and the name of the patch-file, which contains instructions for replacing bytes in the binary file (in the format <offset>: <byte value located at the given address>).

```
0120: 90
0121: 90
```

For example, the program will place two NOP operations at offsets 0120h and 0121h following this instruction.

The program reads the contents of the binary file into the buffer, replaces it according to the instructions in the patch file, and overwrites the binary file.

These operations are accompanied by an 8-bit NOKIA-3310 ringtone and a hacking animation:


<img src="/patch/screensAndSound/screen0.bmp" alt="image" width="400" height="600">


### Using for hacking

Using the IDA disassembler on my partner's .com file, I noticed that at addresses 0120h and 0121h there is a conditional jump operation jnz, which redirects the program to output the phrase "access denied" if the password hashes do not match:


<img src="readmeImgs/image1.png" alt="image" width="1000" height="500">


I placed the following instructions in the file "patch.txt":

```
0120: 90
0121: 90
```

Using a patcher program, I replaced the conditional jump operation with two NOP operations:


<img src="readmeImgs/image2.png" alt="image" width="1000" height="500">


The hacked program displays the message "access approved" when entering any password:


<img src="readmeImgs/image3.png" alt="image" width="1000" height="100">
















