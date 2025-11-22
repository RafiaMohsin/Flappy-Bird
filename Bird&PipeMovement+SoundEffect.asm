org 0x0100


jmp start
bird_y      dw 10         ; starting row
bird_x      dw 20         ; starting column
jump_flag   dw 0
gravity_tick db 0
forward_tick db 0
randNum: dw 0
oldisr: dw 0, 0
pipe_x dw 140  ; add this with your other variables

;  Bird
; ===============================
draw_bird:
    push ax
    push bx
    push di

    mov ax, 0xb800
    mov es, ax

    ; compute DI = y*160 + x*2 (offset)
    mov ax, [bird_y]
    mov bx, 160
    mul bx
    mov bx, [bird_x]
    shl bx, 1
    add ax, bx
    mov di, ax

    ; row 1: (•>
    mov ah, 0x30
    mov al, '('
    mov [es:di], ax
    add di, 2

    mov al, 0x07
    mov [es:di], ax
    add di, 2

    mov al, '>'
    mov [es:di], ax

    ; row 2
    sub di, 4
    add di, 160

    mov al, '/'
    mov [es:di], ax
    add di, 2

    mov al, ')'
    mov [es:di], ax

    pop di
    pop bx
    pop ax
    ret
erase_bird:
    push ax
    push bx
    push di

    mov ax, 0xb800
    mov es, ax
    ;5 cell cleaned
    mov ax, [bird_y]
    mov bx, 160
    mul bx
    mov bx, [bird_x]
    shl bx, 1
    add ax, bx
    mov di, ax

    mov ax, 0x3320
    mov [es:di], ax
    add di, 2
    mov [es:di], ax
    add di, 2
    mov [es:di], ax

    sub di, 4
    add di, 160

    mov [es:di], ax
    add di, 2
    mov [es:di], ax

    pop di
    pop bx
    pop ax
    ret

;  KEYBOARD ISR
; ===============================
kbisr:
    push ax
    in al, 0x60  ; keypress
    cmp al, 0x39      ; spacebar
    jne .pass

    mov word [jump_flag], 1
    jmp .done
.pass:
    pop ax
    jmp far [oldisr]
.done:;EOI
    mov al, 0x20
    out 0x20, al
    pop ax
    iret


; ===============================
;  DELAY,RAND,CLR
; ===============================
bird_delay:
    mov cx, 18000
    .wait:
    loop .wait
    ret
pipe_delay:
    push cx
    mov cx, 0xffff
    _:  nop
    loop _
    pop cx
    ret
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
.clear:
    mov [es:di], ax
    add di, 2
    loop .clear
    ret

;  Pipes
; ===============================
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

    add di, 1600 ; 10 lines gap ;)

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

play_sound:
    push ax
    push dx

    ; set PIT channel 2 to mode 3 (square wave)
    mov al, 0B6h
    out 43h, al

    ; calculate divisor for frequency
    mov ax, 1193180
    div bx
    out 42h, al
    mov al, ah
    out 42h, al

    ; turn on speaker (bits 0 and 1)
    in al, 61h
    or al, 03h
    out 61h, al
 
    ; simple delay loop
.delay_loop:
    loop .delay_loop

    ; turn off speaker
    in al, 61h
    and al, 0FCh
    out 61h, al

    pop dx
    pop ax
    ret

; ===============================
start:
; ===============================

save_old_ISR:
    xor ax, ax
    mov es, ax
    mov ax, [es:9*4]
    mov [oldisr], ax
    mov ax, [es:9*4+2]
    mov [oldisr+2], ax
install_new_ISR
    cli
    mov word [es:9*4], kbisr
    mov word [es:9*4+2], cs
    sti
setup_:
    call clrscr
    call bird_delay
   
    call get_rand_num
    mov di,140
    push di
    push word [randNum]
    call DrawPipes
    call draw_bird
    mov cx,30
game_loop:
    ; Remove old pipe
    call pipe_delay
    call pipe_delay
    push word [pipe_x]
    push word [randNum]
    call RemovePipes
   
   
    ; Move pipe left
    sub word [pipe_x], 2
    ; Check if pipe went off screen
    cmp word [pipe_x], 0
    jge .draw_pipe
    
    ; Reset pipe to right side with new random height
    mov word [pipe_x], 140
    call get_rand_num
    
.draw_pipe:
    push word [pipe_x]
    push word [randNum]
    call DrawPipes
    
    call erase_bird

    ; -----------------------  
    ; GRAVITY
    ; -----------------------
    inc byte [gravity_tick]
    cmp byte [gravity_tick], 1
    jl .skip_gravity
    mov byte [gravity_tick], 0
    add word [bird_y], 1
.skip_gravity:

    ; -----------------------
    ; FORWARD MOVEMENT
    ; -----------------------
    inc byte [forward_tick]
    cmp byte [forward_tick], 10
    jl .skip_forward
    mov byte [forward_tick], 0

    add word [bird_x], 1
    cmp word [bird_x], 20
    jle .skip_forward
    mov word [bird_x], 20
.skip_forward:

    ; -----------------------
    ; JUMP
    ; -----------------------
    cmp word [jump_flag], 1
    jne .no_jump
    push cx
     ; --- PLAY JUMP SOUND ---
mov bx, 950      ; frequency
mov cx, 2500     ; duration
call play_sound
    pop cx
    sub word [bird_y], 4
    mov word [jump_flag], 0
.no_jump:

    ; -----------------------
    ; LIMITS
    ; -----------------------
    cmp word [bird_y], 1
    jl .limit_top
    cmp word [bird_y], 22
    jg .limit_bottom
    jmp .ok
   
.limit_top:
    mov word [bird_y], 1
    jmp .ok

.limit_bottom:
    mov word [bird_y], 22

.ok:
    call draw_bird
    call bird_delay
    jmp game_loop 
    
    
    
    
    