[org 0x0100]
    jmp start


clrscr:
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

    pop di
    pop ax
    pop es

    ret

printnum: 
    push bp
    mov bp, sp
    push es
    push ax
    push bx
    push cx 
    push dx
    push di
    push si
    mov si, 0
    mov ax, 0xb800
    mov es, ax ; point es to video base
    mov ax, [bp+4] ; load number in ax
    cmp ax, 0
    je noPrint

    mov cx, 0 ; initialize count of digits
    nextdigit:
    mov bx, 10
    mov dx, 0 ; zero upper half of dividend
    div bx ; divide by 10
    add dl, 0x30 ; convert digit into ascii value
    push dx ; save ascii value on stack
    inc cx ; increment count of values
    cmp ax, 0 ; is the quotient zero
    jnz nextdigit ; if no divide it again
    mov di,  [bp+6];
    nextpos:
    pop dx ; remove a digit from the stack
    mov dh, 0x34 ; use normal attribute
    mov [es:di], dx ; print char on screen
    add di, 2 ; move to next screen location
    ;inc si
    loop nextpos ; repeat for all digits on stack
    noPrint:
    pop si
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    pop es
    pop bp
    ret 4

get_rand_num:  ;returns a random number between 1 and 14
    push bp
    mov bp,sp;
    push cx
    push ax
    push dx;

    MOV AH, 0h ;uses the system clock ticks as a randomness source.
    INT 1AH ; CX:DX now hold number of clock ticks since midnight
    ;The clock ticks at 18.2 times per second
    mov ax, dx
    xor dx, dx
    ;DX changes very fast So good source of randomness
    mov cx, 14;
    div cx ; this is from 0 to 13 
    inc dx ; now dx : 1 to 14
    mov word [randNum],dx ; randNum updated

    pop dx;
    pop ax;
    pop cx;
    pop bp;
    ret
;0DBh

DrawPipes:
    push bp
    mov bp, sp
    pusha


    mov ax, 0xb800
    mov es, ax

;starting position 
    mov di, [bp+6]
;-------pipes
    mov ah,30h
    mov al, 0BAh
;height
    mov bx, [bp+4]
upperPipe:
 mov cx, 8
drawChar_: ; this will draw 8 char in one row :)
    mov [es:di], ax
    add di, 2
    loop drawChar_
    add di, 144  ;(80 columns * 2 bytes) - (8 * 2 ) // next line.
    sub bx, 1    ; height decreases gradually..
    cmp bx, 0
jg upperPipe

add di, 1280 ; 8 lines gap ;)

LowerPipe:
    mov cx, 8
_drawChar:
    mov [es:di], ax
    add di, 2
    loop _drawChar
    add di, 144
    cmp di, 4000
jl LowerPipe

    popa
    pop bp
    ret 4

RemovePipes: 
    push bp
    mov bp, sp
    pusha
    ;parameters same as for draw pipe subroutine
    mov ax, 0xb800
    mov es, ax

    mov di, [bp+6]

    mov ah, 0x33
    mov al, ' '

    mov bx, [bp+4]
clearing:
mov cx, 8
removeChar:
mov [es:di], ax
add di, 2
loop removeChar
add di, 144 ; next line
sub bx, 1
cmp di, 4000
jl clearing




popa
pop bp
ret 4

delay:
    push cx
    mov cx, 0xffff
    _:  nop
    loop _
    pop cx
    ret

start:
    call clrscr
   call delay
   
    call get_rand_num
    mov di,140
    push di
    push word [randNum]
    call DrawPipes
    mov cx,30
l:
    call delay
    call delay
    
    
    push di
    push word [randNum]
    call RemovePipes
    
    
    sub di,4 ; 2 cols back 
    push di
    push word [randNum]
    call DrawPipes
    loop l
    
    MOV AH, 4CH
    INT 21H

randNum: dw 0