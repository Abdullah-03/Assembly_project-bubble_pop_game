[org 0x0100] 
 jmp start 
seconds: db 0
skip: dw 0
time: dw 200
prev_tick: dw 0
scroll: dw 0
printed_bubble: dw 0
score: dw 0
total_bubbles: dw 0
bubbles_popped: dw 0

congo: db 'well, played. gg'
len_congo: dw 16

str1: db 'time:  '
str2: db 'score: '
str3: db 'bubbles missed:'
len: dw 7
len2: dw 15

oldisr: dd 0 ; space for saving old isr

; subroutine to print a string 
; takes the x position, y position, attribute, address of string and 
; its length as parameters 

	

	 
printstr: 
push bp 
 mov bp, sp 
 push es 
 push ax 
 push cx 
 push si 
 push di 
 
 mov ax, 0xb800 
 mov es, ax ; point es to video base 
 mov al, 80 ; load al with columns per row 
 mul byte [bp+10] ; multiply with y position 
 add ax, [bp+12] ; add x position 
 shl ax, 1 ; turn into byte offset 
 mov di,ax ; point di to required location 
 mov si, [bp+6] ; point si to string 
 mov cx, [bp+4] ; load length of string in cx 
 mov ah, [bp+8] ; load attribute in ah
 
 cld ; auto increment mode 
nextchar: lodsb ; load next char in al 
 stosw ; print char/attribute pair 
 loop nextchar ; repeat for the whole string 
 pop di 
 pop si 
 pop cx 
 pop ax 
 pop es 
 pop bp 
 ret 10
 

	delay: 
	push cx
	mov cx, 0xFFFF

		step: loop step
		pop cx
	ret



print_rectangle:
  push bp
  mov bp , sp
  
  push ax
  push si
  push cx
  push es
  push di
  push dx

  push word[bp + 12]
  call color_checker
  mov dh  , ah
  mov dl , 0x20

  ;Storing index of top left coordinate in SI register
  push word[bp + 10]
  push word[bp + 8]
  call converter
  mov si  , ax

  ;Storing index of bottom right coordinate in DI register
  push  word[bp + 10]
  push word[bp +4]
  call converter

  mov di , ax

  ;storing the width of rectangle in Cx
  mov cx , [bp + 6]
  sub cx , [bp + 10]
  
  ;printing parallel horizontal lines of rectangle
  mov ax , 0xb800
  mov es , ax

  mov ax , si  ; storing the si in ax 

  l1:
    mov word[es: si] ,dx 
    mov word[es:di] , dx
    add di , 2
    add si , 2
   
    

    sub cx , 1
    jnz l1
    


  ;storing the length of rectangle in Cx
   mov cx , [bp +4]
   sub cx , [bp + 8]


 ;Storing index of top right coordinate in DI register
   mov si , ax       ; restoring the value on si as stored in ax previously (top left coordinate)
   push  word[bp + 6]
   push word[bp +8]
   call converter
   mov di , ax
  

   
  ; printing vertical parallel lines of rectangle 
    l2:
    mov word[es:di] , dx
    mov word[es: si] ,dx
    
    add si , 160
    add di , 160

    sub cx , 1
    jnz l2
    mov word[es:di] , dx


  ; 2d top left cord turned into 1d coordinates
  push word[bp + 10]
  push word[bp + 8]
  call converter
  mov si  , ax

  ; 2d top right cord turned into 1d coordinates
  push  word[bp + 10]
  push word[bp +4]
  call converter
  mov di , ax
  
  ;storing the width of rectangle in bx
  mov bx , [bp + 6]
  sub bx , [bp + 10]
  shl bx, 1
  
  mov word[es:di] , ' '
	
    mov word[es:si] , ' '

 mov word[es:di + bx] , ' '
	
    mov word[es:si + bx] , ' '
	
	
	;print letter in bubble
	xor ax,ax            ; xor register to itself same as zeroing register
	int 0x1A             ; Int 1ah/ah=0 get timer ticks since midnight in CX:DX
	mov ax,dx            ; Use lower 16 bits (in DX) for random value
	
	xor dx,dx            ; Compute randval(DX) mod 10 to get num
	mov bx,26            ;     between 0 and 120
	div bx               ; Divide dx:ax by bx
	mov ax,dx
	
	add al, 'a'
	mov ah, 0x07
	mov word[es:si+326], ax
   
	add word [total_bubbles], 1	
	
  pop dx
  pop di
  pop es
  pop cx
  pop si
  pop ax
  pop bp

  ret 10




color_checker:
    push bp
    mov bp , sp

    mov ah , 0x47
    cmp word[bp + 4] , 1
    jnz second
    mov ah , 0x17
    jmp end

    second:
        cmp word[bp + 4] , 2
        jnz third
        mov ah , 0x47
        jmp end
    third:
        cmp word[bp + 4] , 3
        jnz end
        mov ah , 0x27

    
    end:
        pop bp
        ret 2

converter:
 push bp
 mov bp , sp
 
 push bx
 mov ax , 0
 mov al , 80
 mul byte[bp + 4]
 add ax , [bp + 6]
 shl ax , 1


 pop bx
 pop bp
 
 ret 4

 
	clearscreen:
    push ax
    push es
    push di

    mov ax , 0xb800
    mov es , ax

    mov di , 0

    next:

    mov word[es : di] , 0x0720
    add di , 2
    cmp di , 4000
    jnz next

    pop di
    pop es
    pop ax
    ret




; take the number of lines to scroll as parameter 
scrollup: push bp 
 mov bp,sp 
 push ax 
 push cx 
 push si 
 push di 
 push es 
 push ds 
 mov ax, 80
 mov bx,0
 mov bl, [bp + 4]   ;number of lines scrolled up
 mul bl 
 mov si, ax 
 push si 
 shl si, 1 
 mov cx, 2000 
 sub cx, ax 
 mov ax, 0xb800 
 mov es, ax
 mov ds, ax 
 xor di, di 
 cld 
 rep movsw  
 mov ax, 0x0720 
 pop cx
 rep stosw  
 pop ds 
 pop es 
 pop di 
 pop si 
 pop cx 
 pop ax 
 pop bp 
 ret 2


; takes the number to be printed as its parameter 
printnum: 
push bp 
	mov bp, sp 
	 push es 
	 push ax 
	 push bx 
	 push cx 
	 push dx 
	 push di 
	 
	 
	 mov ax, 0xb800 
	 mov es, ax ; point es to video base 
	 mov ax, [bp+4] ; load number in ax 
	 mov bx, 10 ; use base 10 for division 
	 mov cx, 0 ; initialize count of digits 
	 
	nextdigit: mov dx, 0 ; zero upper half of dividend 
	 div bx ; divide by 10 
	 add dl, 0x30 ; convert digit into ascii value 
	 push dx ; save ascii value on stack 
	 inc cx ; increment count of values 
	 cmp ax, 0 ; is the quotient zero 
	 jnz nextdigit ; if no divide it again 
	 
	 
	 mov di, [bp + 6] ; point di to given coords
	 
	 mov word[es:di], 0x0720
	 mov word [es:di + 2], 0x0720
	 mov word [es:di + 4], 0x0720 
	 
	 nextpos:
	 pop dx ; remove a digit from the stack 
	 mov dh, 0x07 ; use normal attribute 
	 mov [es:di], dx ; print char on screen 
	 add di, 2 ; move to next screen location 
	 loop nextpos ; repeat for all digits on stack
	 
	 
	 pop di 
	 pop dx 
	 pop cx 
	 pop bx 
	 pop ax 
	 pop es 
	 pop bp 
	 ret 2


kbisr: 
	push ax 
	 push es 
	 push si
	 
	 mov ax, 0xb800 
	 mov es, ax ; point es to video memory 
	 in al, 0x60 ; read a char from keyboard port
	 
	 call convert_scan_to_ascii
	 
	mov ah, 0x07
	xor si, si
	sub si, 2
	
	search_video_mem:
	add si, 2
	
	 cmp si, 4000
	 jg no_match
	 
	 cmp ax, [es:si]; search for key 
	 jne search_video_mem ; no, try next comparison 
	 
	 mov word[es:si], 0x0720
	 
	  mov word [es:si-320], 0x0720
	 mov word [es:si-322], 0x0720
	 mov word [es:si-324], 0x0720
	 mov word [es:si-318], 0x0720
	 mov word [es:si-316], 0x0720
	 ;call delay
	 
	mov word [es:si+320], 0x0720
	mov word [es:si+322], 0x0720
	mov word [es:si+324], 0x0720
	mov word [es:si+318], 0x0720
	mov word [es:si+316], 0x0720
	;call delay
	
	 mov word [es:si+6], 0x0720
	 mov word [es:si+166], 0x0720
	mov word [es:si+-154], 0x0720
	;call delay
	
	mov word [es:si-6], 0x0720
	mov word [es:si-166], 0x0720 
	 mov word [es:si+154], 0x0720
	 
	 add word [score], 10
	 add word [bubbles_popped], 1

	no_match: 
	mov al, 0x20 
	 out 0x20, al ; send EOI to PIC 
	 pop si
	 pop es 
	 pop ax 
	 iret


convert_scan_to_ascii:
	
	cmp al, 0x1e
	jne b
	mov al, 'a'
	ret
	
b:	cmp al, 0x30
	jne c
	mov al, 'b'
	ret
	
c:	cmp al, 0x2e
	jne d
	mov al, 'c'
	ret
	
d:	cmp al, 0x20
	jne e
	mov al, 'd'
	ret
	
e:	cmp al, 0x12
	jne f
	mov al, 'e'
	ret
	
f:	cmp al, 0x21
	jne g
	mov al, 'f'
	ret
	
g:	cmp al, 0x22
	jne h
	mov al, 'g'
	ret
	
h:	cmp al, 0x23
	jne i
	mov al, 'h'
	ret
	
i:	cmp al, 0x17
	jne j
	mov al, 'i'
	ret
	
j:	cmp al, 0x24
	jne k
	mov al, 'j'
	ret
	
k:	cmp al, 0x25
	jne l
	mov al, 'k'
	ret
	
l:	cmp al, 0x26
	jne m
	mov al, 'l'
	ret
	
m:	cmp al, 0x32
	jne n
	mov al, 'm'
	ret
	
n:	cmp al, 0x31
	jne o
	mov al, 'n'
	ret

o:	cmp al, 0x18
	jne p
	mov al, 'o'
	ret
	
p:	cmp al, 0x19
	jne q
	mov al, 'p'
	ret	
	
q:	cmp al, 0x10
	jne r
	mov al, 'q'
	ret	
	
r:	cmp al, 0x13
	jne s
	mov al, 'r'
	ret	
	
s:	cmp al, 0x1f
	jne t
	mov al, 's'
	ret
	
t:	cmp al, 0x14
	jne u
	mov al, 't'
	ret
	
u:	cmp al, 0x16
	jne v
	mov al, 'u'
	ret
	
v:	cmp al, 0x2f
	jne w
	mov al, 'v'
	ret
	
w:	cmp al, 0x11
	jne x
	mov al, 'w'
	ret
	
x:	cmp al, 0x2d
	jne y
	mov al, 'x'
	ret
	
y:	cmp al, 0x15
	jne z
	mov al, 'y'
	ret
	
z:	cmp al, 0x2c
	jne no_ascii
	mov al, 'z'
	ret
	
no_ascii:
	mov al, 00
	ret
	
start:
 call clearscreen
 
 xor ax, ax 
 mov es, ax ; point es to IVT base 
 mov ax, [es:9*4] 
 mov [oldisr], ax ; save offset of old routine 
 mov ax, [es:9*4+2] 
 mov [oldisr+2], ax ; save segment of old routine 
 cli ; disable interrupts 
 mov word [es:9*4], kbisr ; store offset at n*4 
 mov [es:9*4+2], cs ; store segment at n*4+2 
 sti

main_loop:
	
	
	while:   
	;GET SYSTEM TIME.
	mov  ah, 2ch
	int  21h ;RETURN SECONDS IN DH.
	;CHECK IF ONE SECOND HAS PASSED. 
	cmp  dh, [seconds]
	je   no_change
	;IF NO JUMP, ONE SECOND HAS PASSED. VERY IMPORTANT : PRESERVE SECONDS TO
	;USE THEM TO COMPARE WITH NEXT SECONDS. THIS IS HOW WE KNOW ONE SECOND
	;HAS PASSED.

	mov  [seconds], dh
	dec word [time]
	;cmp word [delay], 5
	;jbe scroll
	
	no_change:
	
	xor ax,ax            ; xor register to itself same as zeroing register
	int 0x1A             ; Int 1ah/ah=0 get timer ticks since midnight in CX:DX
	
	sub dx, [prev_tick]
	cmp dx, 2			;control smoothness of scroll
	jae frame
	
	
	jmp while
	
	
	
	
	frame:
	
	add dx, [prev_tick]
	mov word [prev_tick], dx
	
	push 1
	call scrollup
	
	inc word [scroll]
	cmp word [scroll], 15
	jb no_bubble
	
	;print new bubble every 15 frames
	 mov word [scroll], 0 
xor ax,ax            ; xor register to itself same as zeroing register
int 0x1A             ; Int 1ah/ah=0 get timer ticks since midnight in CX:DX
mov ax,dx            ; Use lower 16 bits (in DX) for random value

xor dx,dx            ; Compute randval(DX) mod 10 to get num
mov bx,100           ;     between 0 and 120
div bx               ; Divide dx:ax by bx
sub dx, 12

no1:
	
	push 1;	  color input 1-Blue , 2- Red , 3- Green			 
	push dx  ;top_left x
	push 19 ; top_left y
	
	add dx, 6
	push dx  ; bot_right x
	push 23  ; bot_right y
	call print_rectangle
	
;no2:

	;push 1;	  color input 1-Blue , 2- Red , 3- Green			 
	;push 120  ;top_left x
	;push 19 ; top_left y
;
;	push 126  ; bot_right x
;	push 23  ; bot_right y
;	call print_rectangle
	
;	mov word [printed_bubble], 0
	
	
	no_bubble:
	
	cmp word [time], 0
	jle _end
	
	push 0
	push 0
	push 2
	push str1
	push word [len]
	call printstr
	
	push 16
	push word [time]
	call printnum 
	
	push 0
	push 1
	push 2
	push str2
	push word [len]
	call printstr
	
	push 176
	push word [score]
	call printnum 
	
	
  jmp  while
 
_end: 
  call clearscreen
  
	push 24
	push 6
	push 3
	push congo
	push word [len_congo]
	call printstr
  
	push 30
	push 12
	push 2
	push str2
	push word [len]
	call printstr
	
	push 1994
	push word [score]
	call printnum 
	
	push 30
	push 13
	push 2
	push str3
	push word [len2]
	call printstr
	
	mov ax, [total_bubbles]
	sub ax, [bubbles_popped]
	dec ax
	
	push 2172
	push ax
	call printnum 

	mov ax, [oldisr] ; read old offset in ax 
 mov bx, [oldisr+2] ; read old segment in bx 
 cli ; disable interrupts 
mov [es:9*4], ax ; restore old offset from ax 
 mov [es:9*4+2], bx ; restore old segment from bx 
 sti
 
 
mov ah,0x1
int 0x21 
 mov ax, 0x4c00 ; terminate program 
 int 0x21