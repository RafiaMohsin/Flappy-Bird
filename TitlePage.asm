[org 0x0100]
jmp start

printstr:
push bp
mov bp, sp
pusha

push ds
pop es
mov di,[bp+4]
mov cx, 0xffff
xor al,al
repne scasb
mov ax, 0xffff
sub ax, cx
dec ax
jz d

mov cx, ax
mov ax, 0xb800
mov es, ax
mov di, [bp+6]
mov si, [bp+4]
mov ah, 0x30
cld
nextchar:
lodsb
stosw
loop nextchar
d:
popa
pop bp
ret 4

logo:
    push bp
    mov bp,sp
    pusha

    mov di, [bp+4]
    mov ah, 0xB0
    mov al, '('
    mov [es:di], ax
    add di, 2

    ; '•'
    mov al, 0x07   ; 
                  
    mov [es:di], ax
    add di, 2
    ; '>'
    mov al, '>'
    mov [es:di], ax
    ; go to next line (160 - 6 = 154)
    add di, 154
    ; '/'
    mov al, '/'
    mov [es:di], ax
    add di, 2

    ; ')'
    mov al, ')'
    mov [es:di], ax
    popa
    pop bp
    ret 2

titleScr:
push es
push ax
push cx
push di
mov ax,0xb800
mov es,ax
xor di,di
mov ax,0x3320
mov cx,2000
cld
rep stosw

mov ax, TitleLine1
push  490     ;160*3 + 5*2
push ax
call printstr

mov ax, TitleLine1
push  3530     ;160*22 + 5*2
push ax
call printstr

mov ax, TitleLine2
push 1030
push ax
call printstr

mov ax, BirdLine1
push 160*9 + 30*2
push ax
call printstr

mov ax, BirdLine2
push 160*10 + 30*2
push ax
call printstr

mov ax, BirdLine1
push 160*9 + 50*2
push ax
call printstr

mov ax, BirdLine2
push 160*10 + 50*2
push ax
call printstr

mov ax, welcome1
push 1978
push ax
call printstr

mov ax, welcome2
push 2466
push ax
call printstr

mov ax, StartMsg
push 2948
push ax
call printstr
mov di, 2040
 push di
 call logo

mov di, 1956
 push di
 call logo

pop di
pop cx
pop ax
pop es
ret
start:
call titleScr
MOV AH, 4CH
INT 21H
TitleLine1  db "=====================================================================",0
TitleLine2  db "FLAPPY BIRD",0
BirdLine1   db "(.>",0
BirdLine2   db "/)",0
StartMsg    db "!FLAP!  !FLAP!",0
welcome1: db "!HELP THE BIRD FLY HIGH!",0
welcome2 : db "AVOID THE PIPES!",0
