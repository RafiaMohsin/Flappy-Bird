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



endscr:
push es
push ax
push di
mov ax, 0xb800
mov es, ax
mov di, 0

mov ax, 0x3320
mov cx, 2000
cld
rep stosw

mov ax, game_over
    push word 1990
    push ax
    call printstr

pop di
pop ax
pop es

ret

start:
    call endscr


    MOV AH, 4CH
    INT 21H

game_over : db "Game Over!",0