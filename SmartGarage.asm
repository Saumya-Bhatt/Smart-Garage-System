org 100h

data segment
;variables to keep track of port data
	portb_val db 0
    portc_val db 0
    porta_val db 0
   
    
    empty db 'empty'
	full db 'full'
	count dw 0
    
;port addresses  
    porta equ 00h
	portb equ 02h 	;portb is connected to the d7-d0
	portc equ 04h 	;portc0 is rw, portc1 is rs, portc2 is en
	pcw   equ 06h	;port for io control
    
	timer_clock equ 08h
    timer_remote equ 0ah
    timer_door equ 0ch
    creg_timer equ 0eh
	
    timer_clock2 equ 10h
    timer_remote2 equ 12h
    timer_door2 equ 14h
    creg_timer2 equ 16h
	jmp st1
    db 1024 dup(0)

.code
.startup
      
st1: cli



start:
; set segment registers: 
    mov ax, data
    mov ds, ax
    mov es, ax
	mov ss,ax
    mov sp,0FFFEH
    mov si,0000
    
;define io ports
    mov dx,pcw
    mov al,10010000b   ;to make all ports output
    out dx,al  
    
    mov al, 00110110b   ;counter0,timer1
    out creg_timer, al
    mov al, 0a8h  ;10101000
    out timer_clock, al  ;timer 1 intialize
    mov al, 61h   ;01100001
    out timer_clock, al         

   
   mov al,00110011b       ;timer 2 intialize,counter0
   out creg_timer2,al
   call lcd_init
   call lcd_update	
	
garageclosed:
    in al, porta    
    and al, 00000001b	;remote bit
    cmp al, 1
    je opendoor 
    jmp garageclosed

garageopen:
    mov cl,0
    mov ah, 0                   ; reset car flag to 0     
    in  al, porta
	mov bl, al       
    and bl, 00000001b
    cmp bl, 00000001b           ; check for remote press      
    je closedoor
    mov bl, al       
    and bl, 00010000b
    cmp bl, 00010000b           ; check for timeout (5 minutes)
    je closedoor
    mov bl, al       
    and bl, 00000010b
    cmp bl, 00000010b           ; check for outer ir
    je entering    
    mov bl, al       
    and bl, 00001000b
    cmp bl, 00001000b           ; check for inner ir
    je exiting    
    jmp garageopen    
    
closedoor:
    call motor_clockwise
    call motor_start 
    call start_door_timer
    stillclosing:
        in al, porta
        and al, 00100000b
        cmp al, 00100000b       ; wait for door to close completely
        jne stillclosing 
        
    call motor_stop
    jmp garageclosed

opendoor:
    call start_remote_timer
    call motor_anticlockwise     
    call motor_start 
    call start_door_timer
	
    stillopening:
        in al, porta
        and al, 00100000b
        cmp al, 00100000b       ; wait for door to open completely
        jne stillopening
        
    call motor_stop
    jmp garageopen

entering:
    mov cl,0       
    in al, porta
    mov bl, al       
    and bl, 00000001b		;remote bit
    cmp bl, 00000001b                 
    je closedoor
    mov bl,al	
    and bl, 00010000b
    cmp bl, 00010000b           ; check for timeout (5 minutes)
    je closedoor
    mov bl, al       
    and bl, 00000100b
    cmp bl, 00000100b           ; check for car: transducer bit
    jne nc01                     ;no car entering at that moment
    mov ah, 1
	
    nc00:    
        mov bl, al       
        and bl, 00001000b
        cmp bl, 00001000b       ; check for inner ir
        jne entering
		

    cmp ah, 1
    jne nc01
    inc count            
    call lcd_update
    nc01: 
        in al, porta
        mov bl, al       
        and bl, 00001000b
        cmp bl, 00001000b       ; debounce
        je nc01
    jmp garageopen

exiting:  
    mov cl,0     
    in  al, porta
    mov bl, al
    mov bl, al       
    and bl, 00000001b
    cmp bl, 00000001b                
    je closedoor
	
    and bl, 00010000b
    cmp bl, 00010000b           ; check for timeout (5 minutes)
    je closedoor
	
    mov bl, al       
    and bl, 00000100b
    cmp bl, 00000100b           ; check for car
    jne nc10
	
    mov ah, 1
    nc10:    
    mov bl, al       
        and bl, 00000010b
        cmp bl, 00000010b       ; check for outer ir
        jne exiting
    cmp ah, 1
    jne nc11
    dec count 
	
    call lcd_update
    nc11:        
        in al, porta
        mov bl, al       
        and bl, 00000010b
        cmp bl, 00000010b       ; debounce
        je nc11
    jmp garageopen
	
	mov dl,1
	mov dh,1
	call lcd_set_cur
	
	
	  	
  
	hlt
;end of main procedure

lcd_update proc near
    call lcd_clear
    mov al, ' '
    
    cmp count, 0
    jnz notempty
	mov dl,2
	mov dh,1
	call lcd_set_cur
	lea si,empty
	call lcd_printstr
	jmp xx
    notempty:
        cmp count, 2000
        jnz notfull
		mov dl,2
		mov dh,1
		call lcd_set_cur
		lea si,full
		call lcd_printstr
		jmp xx
			notfull:
    		mov dl,2
			mov dh,1
			call lcd_set_cur
			lea si,count+'0'
			call lcd_printstr
     xx:     
	 ret
	
lcd_update endp


start_door_timer proc near
        mov al, 10110000b
        out creg_timer, al  
        mov al, 90h
        out timer_door, al
        mov al, 01h
        out timer_door, al      
        ret
start_door_timer endp

start_remote_timer proc near
        mov al, 01110000b
        out creg_timer, al
        mov al, 30h
        out timer_remote, al
        mov al, 75h
        out timer_remote, al      
        ret
start_remote_timer endp

motor_stop proc near
        in al, portc
        and al, 00111111b  
        or al, 00000000b
        out portc, al       
        ret
motor_stop endp

motor_anticlockwise proc near     ;open door
        in al, portc
        and al, 00111111b  
        or al, 01000000b
        out portc, al
        ret       
motor_anticlockwise endp

motor_clockwise proc near         ;close door
        in al, portc
        and al, 00111111b  
        or al, 10000000b
        out portc, al       
        ret
motor_clockwise endp

motor_start proc near
       	in al,0cah
       	out timer_clock2,al
       	in al,08h
       	out timer_clock2,al
       	in al,0d4h
       	out timer_clock2,al
       	in al,30h
       	out timer_clock2,al
        ret
motor_start endp


proc delay
;input: cx, this value controls the delay. cx=50 means 1ms
;output: none
	jcxz delay_end
	del_loop:
	loop del_loop	
	delay_end:
	ret
endp delay



; lcd initialization
proc lcd_init
;input: none
;output: none

;make rs=en=rw=0
	mov al,0
	call out_b
;delay 20ms
	mov cx,1000
	call delay
;reset sequence
	mov ah,30h
	call lcd_cmd
	mov cx,250
	call delay
	
	mov ah,30h
	call lcd_cmd
	mov cx,50
	call delay
	
	mov ah,30h
	call lcd_cmd
	mov cx,500
	call delay
	
;function set
	mov ah,38h
	call lcd_cmd
	
	mov ah,0ch
	call lcd_cmd
	
	mov ah,01h
	call lcd_cmd
	
	mov ah,06h
	call lcd_cmd
	
	ret	
endp lcd_init




;sends commands to lcd
proc lcd_cmd
;input: ah = command code
;output: none

;save registers
	push dx
	push ax
;make rs=0
	mov al,portc_val
	and al,0fdh		;en-rs-rw
	call out_b
;set out data pins
	mov al,ah
	call out_a
;make en=1
	mov al,portc_val
	or	al,100b		;en-rs-rw
	call out_b
;delay 1ms
	mov cx,50
	call delay
;make en=0
	mov al,portc_val
	and al,0fbh		;en-rs-rw
	call out_b
;delay 1ms
	mov cx,50
	call delay
;restore registers
	pop ax
	pop dx	
	ret
endp lcd_cmd




proc lcd_clear
	mov ah,1
	call lcd_cmd
	ret	
endp lcd_clear



;writes a character on current cursor position
proc lcd_write_char
;input: ah
;output: none

;save registers
	push ax
;set rs=1
	mov al,portc_val
	or	al,10b		;en-rs-rw
	call out_b
;set out the data pins
	mov al,ah
	call out_a
;set en=1
	mov al,portc_val
	or	al,100b		;en-rs-rw
	call out_b
;delay 1ms
	mov cx,50
	call delay
;set en=0
	mov al,portc_val
	and	al,0fbh		;en-rs-rw
	call out_b
;return
	pop ax
	ret	
endp lcd_write_char





;prints a string on current cursor position
proc lcd_printstr
;input: si=string address, string should end with '$'
;output: none

;save registers
	push si
	push ax
;read and write character
	@lcd_printstr_lt:
		lodsb
		cmp al,'$'
		je @lcd_printstr_exit
		mov ah,al
		call lcd_write_char	
	jmp @lcd_printstr_lt
	
;return
	@lcd_printstr_exit:
	pop ax
	pop si
	ret	
endp lcd_printstr




;sets the cursor
proc lcd_set_cur
;input: dl=row, dh=col
;		dl = 1, means upper row
;		dl = 2, means lower row
;		dh = 1-8, 1st column is 1
;output: none

;save registers
	push ax
;lcd uses 0 based column index
	dec dh
;select case	
	cmp dl,1
	je	@row1
	cmp dl,2
	je	@row2
	jmp @lcd_set_cur_end
	
;if dl==1 then
	@row1:
		mov ah,80h
	jmp @lcd_set_cur_endcase
	
;if dl==2 then
	@row2:
		mov ah,0c0h
	jmp @lcd_set_cur_endcase
		
;execute the command
	@lcd_set_cur_endcase:	
	add ah,dh
	call lcd_cmd
	
;exit from procedure
	@lcd_set_cur_end:
	pop ax
	ret
endp lcd_set_cur






proc lcd_show_cur
;input: none
;output: none
	push ax
	mov ah,0fh
	call lcd_cmd
	pop ax
	ret
endp lcd_show_cur




proc lcd_hide_cur
;input: none
;output: none
	push ax
	mov ah,0ch
	call lcd_cmd
	pop ax
	ret
endp lcd_hide_cur



;sends data to output port and saves them in a variable
proc out_a
;input: al
;output: portb_val
	push dx
	mov dx,portb
	out dx,al
	mov portb_val,al
	pop dx
	ret	
endp out_a


proc out_b
;input: al
;output: portc_val	
	push dx
	mov dx,portc
	out dx,al
	mov portc_val,al
	pop dx
	ret
endp out_b

proc out_c
;input: al
;output: porta_val	
	push dx
	mov dx,porta
	out dx,al
	mov porta_val,al
	pop dx
	ret
endp out_c

ret




