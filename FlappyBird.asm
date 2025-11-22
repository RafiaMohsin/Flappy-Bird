org 0x0100


jmp start
bird_y      dw 10         ; starting row
bird_x      dw 20         ; starting column
jump_flag   dw 0
gravity_tick db 0
crash: dw 0  
score_flag: dw 0
score_val: dw 0
score_str: db "SCORE: ", 0
randNum: dw 0
oldisr: dw 0, 0
pipe_x dw 140  ; add this with your other variables

TitleLine1  db "=====================================================================",0
TitleLine2  db "FLAPPY BIRD",0
BirdLine1   db "(.>",0
BirdLine2   db "/)",0
StartMsg    db "!FLAP!  !FLAP!",0
welcome1: db "!HELP THE BIRD FLY HIGH!",0
welcome2 : db "AVOID THE PIPES!",0

game_over : db "Game Over!",0

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
    mov ah, 0x31
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
    jz nostr

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
    nostr:
    popa
    pop bp
    ret 4
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
    
 


;COLLISION
;===============================
check_collision:
    push ax
    push bx
    push cx
    
    ; SCREEN BOUNDARY CHECK 
    cmp word [bird_y], 1
    jle collision
    
    cmp word [bird_y], 22
    jge collision
    
    ;  PIPE COLLISION CHECK 
    ; Convert pipe_x (byte offset) to column number
    mov ax, [pipe_x]
    shr ax, 1           ; pipe_x / 2 = column number
    
    ; Bird is at column 20-22 (3 chars wide, 2 rows tall)
    ; Pipe is 8 chars wide starting at column 'ax'
    ; Check if pipe overlaps with bird horizontally
    
    ; Pipe left edge
    mov cx, ax          ; CX = pipe column
    
    ; Pipe right edge is at cx+7 (8 chars wide)
    add ax, 7           ; AX = pipe right edge
    
    ; Bird left edge = 20, right edge = 22
    ; Check if bird and pipe overlap horizontally
    cmp cx, 23          ; Is pipe completely to the right of bird?
    jg noCollision
    ; Pipe is not in Bird range
    cmp ax, 19          ; Is pipe completely to the left of bird?
    jl noCollision
    
    ; === VERTICAL COLLISION (if horizontally overlapping) ===
    ; Upper pipe goes from row 0 to row [randNum]
    ; Gap is from row [randNum] to row [randNum+10]
    ; Lower pipe goes from row [randNum+10] to row 24
    
    ; Bird occupies rows [bird_y] and [bird_y+1]
    mov bx, [bird_y]        ; Bird top
    mov cx, bx
    ;inc cx                  ; Bird bottom = bird_y + 1
    
    ; Check if bird top is above gap (hitting upper pipe)
    cmp bx, [randNum]
    jl collision
    
    ; Check if bird bottom is below gap (hitting lower pipe)
    mov ax, [randNum]
    add ax, 10              ; Gap ends at randNum + 10
    cmp cx, ax
    jge collision           ; Bird bottom >= gap_bottom means collision
    inc word [score_flag]
    cmp  word [score_flag],12
    jl noCollision
    mov word [score_flag],0
    inc word [score_val]
    mov bx, 20
   mov cx, 5000
   call play_sound
    ; Bird is inside the gap - safe!
    
noCollision:
    pop cx
    pop bx
    pop ax
    ret
    
collision:
    mov word [crash], 1
    pop cx
    pop bx
    pop ax
    ret
    
;================================
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
    

    mov di, 1360
    push di
    call logo

    pop di
    pop cx
    pop ax
    pop es
    ret
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
    
    mov ax, TitleLine1
    push  490     ;160*3 + 5*2
    push ax
    call printstr

    mov ax, TitleLine1
    push  3530     ;160*22 + 5*2
    push ax
    call printstr
    

    
    mov ax, score_str
    push word 1672
    push ax              ; Top-left corner
    call printstr
    push word 1684
    push word [score_val]            
    call printnum
    mov ax, game_over
    push word 1990
    push ax
    call printstr
    
    mov di, 1854
    push di
    call logo

    
    
    pop di
    pop ax
    pop es

    ret
    
    
; ===============================
start:
; ===============================
    call clrscr
     
    call titleScr
    mov ah,0
    int 0x16
    call clrscr
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

    
    call bird_delay
   
    call get_rand_num
    mov di,140
    push di
    push word [randNum]
    call DrawPipes
    call draw_bird
    mov cx,30
    
game_loop:
    mov ax, score_str
    push word 0
    push ax              ; Top-left corner
    call printstr
    ; Remove old pipe
    ;call pipe_delay
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
    cmp byte [gravity_tick], 2
    jl .skip_gravity
    mov byte [gravity_tick], 0
    add word [bird_y], 1
.skip_gravity:

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
    
    
    call check_collision
    cmp word [crash], 0
    jne isCrash
    
    
    
    push word 14
    push word [score_val]            
    call printnum
    isCrash:
    cmp word [crash], 1
    jne game_loop 
The_end:
; --- PLAY CRASH SOUND ---
; --- GLITCHY CRASH ---
mov bx, 800
mov cx, 2000
call play_sound 
call pipe_delay
mov bx, 200
mov cx, 2000
call play_sound
call pipe_delay
mov bx, 600
mov cx, 2000
call play_sound
    call clrscr
    call endscr
    MOV AH, 4CH
    INT 21H

    
