org 0x0100


jmp start
bird_y      dw 10         ; starting row
bird_x      dw 20         ; starting column
jump_flag   dw 0
gravity_tick db 0
forward_tick db 0

oldisr: dw 0, 0

; ===============================
;  CLEAR SCREEN
; ===============================
clrscr:
    mov ax, 0xb800
    mov es, ax
    mov ax, 0x0720
    mov di, 0
    mov cx, 2000
.clear:
    mov [es:di], ax
    add di, 2
    loop .clear
    ret

  
; ===============================
;  DRAW 2×3 BIRD
; ===============================
draw_bird:
    push ax
    push bx
    push di

    mov ax, 0xb800
    mov es, ax

    ; compute DI = y*160 + x*2
    mov ax, [bird_y]
    mov bx, 160
    mul bx
    mov bx, [bird_x]
    shl bx, 1
    add ax, bx
    mov di, ax

    ; row 1: (•>
    mov ah, 0x0F
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


; ===============================
;  ERASE BIRD
; ===============================
erase_bird:
    push ax
    push bx
    push di

    mov ax, 0xb800
    mov es, ax

    mov ax, [bird_y]
    mov bx, 160
    mul bx
    mov bx, [bird_x]
    shl bx, 1
    add ax, bx
    mov di, ax

    mov ax, 0x0720
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


; ===============================
;  KEYBOARD ISR
; ===============================
kbisr:
    push ax
    in al, 0x60
    cmp al, 0x39      ; spacebar
    jne .pass

    mov word [jump_flag], 1
    jmp .done

.pass:
    pop ax
    jmp far [oldisr]

.done:
    mov al, 0x20
    out 0x20, al
    pop ax
    iret


; ===============================
;  DELAY
; ===============================
delay:
    mov cx, 18000
.wait:
    loop .wait
    ret


; ===============================
;  MAIN
; ===============================
start:

    ; save old ISR
    xor ax, ax
    mov es, ax
    mov ax, [es:9*4]
    mov [oldisr], ax
    mov ax, [es:9*4+2]
    mov [oldisr+2], ax

    ; install new ISR
    cli
    mov word [es:9*4], kbisr
    mov word [es:9*4+2], cs
    sti

    call clrscr

main_loop:
    call erase_bird

    ; -----------------------
    ; GRAVITY
    ; -----------------------
    inc byte [gravity_tick]
    cmp byte [gravity_tick], 12
    jl .skip_gravity
    mov byte [gravity_tick], 0
    add word [bird_y], 1
.skip_gravity:

    ; -----------------------
    ; FORWARD MOVEMENT
    ; -----------------------
    inc byte [forward_tick]
    cmp byte [forward_tick], 4
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
    sub word [bird_y], 6
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
    call delay
    jmp main_loop
