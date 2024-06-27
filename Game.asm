[org 0x0100]

jmp start
mins: dw 9
sec: dw 59
score: dw 0
timeStr: db 'Time: ';6
scoreStr: db 'Score: ';7
endStr:db "Game Over";9
x: dw 32
y: dw 0
flag: db 0; flag to indicate game is over
PrevPos: dw -1; where to remove previous shape: 0 exactly top, 1 top left , 2 top right
shapeNumber: dw 1; 0 square, 1 line, 2 L, 3 J
shapeColor: db 30h

SideChecking:
 push di 
 push dx

   cld
   mov ax,0x7020
	 cmp word [shapeNumber],0
	 je checkCubeInSC
	 cmp word [shapeNumber],1
	 je checkLineInSC
	 cmp word [shapeNumber],2
	 je checkLShapeInSC
	 jmp checkJShapeInSC
   
   checkCubeInSC:
	     mov bx,2
	     cubeInSC:
        cmp [x],dx
        jb skipInCube
        add di,2
        skipInCube: 
          scasw
          jnz noMovement
          add di,158
          scasw
          jnz noMovement
       
          
     jmp exitSideChecking

   checkLineInSC:
	       mov cx,4
		     lineInSC:
	         scasw
           jnz noMovement
		       add di,158
		     loop lineInSC
           
   jmp exitSideChecking

   checkLShapeInSC:
          scasw
          jnz noMovement
          add di,158
          cmp [x],dx
          ja skipInLShape;Do not check element of 2nd row if the right key is pressed
            ;add di,158
            scasw
            jnz noMovement
            sub di,2
          skipInLShape:
          add di,160
          scasw
          jnz noMovement
		     scasw
          jnz noMovement
  jmp exitSideChecking

   checkJShapeInSC:
          scasw
          jnz noMovement
          add di,158
          cmp [x],dx
          jb skipInJShape;Do not check element of 2nd row if the right key is pressed
            ;add di,158
            scasw
            jnz noMovement
            sub di,2
          skipInJShape:
          add di,160
          scasw
          jnz noMovement
          sub di,4
		     scasw
          jnz noMovement
  jmp exitSideChecking
   
  noMovement:
   mov ax,-1

 exitSideChecking:
 
 pop dx
 pop di
ret 

ScrollDown:
 push di
 push ax
 push cx
 push ds




 mov si, di
 mov ax, 0xb800
 mov ds, ax
 ;sub di,160
 sub si,160
 ; mov word [es: di],0x0736


  RepeatInScroll:
    push si
    push di
    mov cx,25
     std 
     rep movsw 
     cld
   pop di
   pop si
   sub si,160
   sub di,160
  cmp si,40
  ja RepeatInScroll


 mov di,40
 mov ax,0x7020
 mov cx,25
 rep stosw
 exitScroll:

 pop ds
 pop cx
 pop ax
 pop di
ret

;This function will reomve the lines stacked at the bottom if the line is complete
RemoveLines:
  push ax
  push dx
  push di
  push bx
  push cx
  ;calculating location
  mov ax,80
  mov dx,[y]
  dec dx
	mul byte dl
	add ax,20
	shl ax,1
  mov di,ax
  ;mov word [es:di],0x0739

    mov ax,0x7020
  mov dx,2
  cmp word [shapeNumber],1
  jb repeatInRL
    cmp word [shapeNumber],1
    ja skipInRL
      add dx,1
    skipInRL:
      add dx,1

  repeatInRL:
     push di
     mov cx,25
     ;checkINRL:
     ;cmp word [es:di],ax
     ;mov word [es:di],0x0732
     ;add di,2
     repne scasw
     jz skipRemoval
     ;loop checkINRL
        
       ;mov ch,0x07
       ;add cl,0x30
   ;    mov bl,cl
  ;     add bl,0x30
    ;   mov bh,07
     ;  mov word [es: di],bx
       sub di,50
       mov cx,25
       rep stosw
     ;  call Delay
       call Delay
      ; call Delay

       sub di,2
       call ScrollDown
       add word [score],10
       call PrintScore
     skipRemoval:
     pop di
     add di,160
     dec dx
  jnz repeatInRL
   
  pop cx
  pop bx
  pop di
  pop dx
  pop ax
ret 

RemoveShapes:
  push di
  mov ax,0x7020
  mov di,[PrevPos]
  cmp di,0
  jl exitRemove
	cmp word [shapeNumber],0
	je RemoveCube
	cmp word [shapeNumber],1
	je RemoveLine
	cmp word [shapeNumber],2
	je RemoveLShape
	jmp RemoveJShape

  RemoveCube:
   	  mov bx,2
	    removeCube:
        mov cx,2
		    rep stosw
        add di,156
	      dec bx
	    jnz removeCube
    jmp exitRemove  

  RemoveLine:
        mov cx,4
        removeLine:
          stosw
          add di,158
        loop removeLine
      jmp exitRemove
  RemoveLShape:
		     mov cx,3
		     removeLshape:
		       stosw
		       add di,158
		     loop removeLshape
		     sub di,158
		     stosw
         jmp exitRemove
  RemoveJShape:
	     mov cx,3
		   removeJshape:
		     stosw
		     add di,158
		   loop removeJshape
		   sub di,162
		   stosw
    

  exitRemove:
  pop di
ret 

LocationChecking:
 push di 
   mov ax,0x7020
	 cmp word [shapeNumber],0
	 je checkCube
	 cmp word [shapeNumber],1
	 je checkLine
	 cmp word [shapeNumber],2
	 je checkLShape
	 jmp checkJShape


   checkCube:
       
      add di,160
      mov ax,0x7020
      mov cx,2
      repe scasw
      jne noPrinting

   jmp exitChecking

   checkLine:
      add di,480
      mov ax,0x7020
      cmp word [es:di],ax
      jne noPrinting
           
   jmp exitChecking

   checkLShape:
      add di,320
      mov ax,0x7020
      mov cx,2
      repe scasw
      jnz noPrinting

  jmp exitChecking

   checkJShape:
      add di,320
      mov ax,0x7020
  ;      mov cx,2
      cmp [es:di],ax
      jnz noPrinting
      sub di,2
      cmp [es:di],ax
      jnz noPrinting

  jmp exitChecking
   noPrinting:
   mov ax,-1

 exitChecking:

 pop di
ret 

PrintShapes:
  
  mov dx,[x];Previous x
  ;Moving on key press
  in al,0x60
	cmp al,0x4b;is key left
	jne nextcmp
	  sub word [x],1
	jmp nomatch
	  nextcmp:
	  cmp al,0x4d;is key right
	  jne nomatch
	  add word [x],1
	nomatch:

    cmp [x],dx
     je skipComparison
   	mov ax,80
	  mul byte [y]
	  add ax,[x]
	  shl ax,1
    mov di,ax

    call SideChecking    
    cmp ax,0
    jge skipComparison
    ;storePreviousX:
    mov [x],dx
    skipComparison:
  ; ;checking for boundries 
  ; cmp word [shapeNumber],3
  ;   jz checkforJ
  ; cmp word [x],20;checking for square,line and L
  ;   jae againcmp
  ;   mov word [x],20
  ; jmp endcmp
  ; checkforJ:
  ; cmp word [x],21
  ;   jae againcmp
  ;   mov word [x],21
  ; jmp endcmp

  ; againcmp:
  ; cmp word [shapeNumber],0
  ;   je checkforSquareandL
  ; cmp word [shapeNumber],2
  ;   je checkforSquareandL
  ; cmp word [x],44
  ;   jbe endcmp
  ;   mov word [x],44
  ; jmp endcmp
  ; checkforSquareandL:
  ; cmp word [x],43
  ;   jbe endcmp
  ;   mov word [x],43
  ; endcmp:

	mov ax,80
	mul byte [y]
	add ax,[x]
	shl ax,1
  mov di,ax
  
  
  call LocationChecking
  cmp ax,0
  jl restartY
  ;checking whether the location is clear or someother shape is already present
  ; mov ax,0x7020
  ; cmp word [shapeNumber],1
  ; je checkLine
  ;   mov cx,2
  ;   repe scasw
  ; jnz restartY
  ; jmp exitcomparison
  ; checkLine:
  ;   scasw
  ; jnz restartY
  
  ; exitcomparison:

  
  call RemoveShapes
  push di
  mov word [PrevPos],di	
  mov ax,[shapeNumber];Specify the type of shape
	cmp ax,0
	je Cube
	cmp ax,1
	je Line
	cmp ax,2
	je LShape
	jmp JShape
  
	  Cube:
		
       pop di
	     mov bx,2
		   mov al,0x20
       mov ah,[shapeColor]
	     cube:
	       mov cx,2
		     rep stosw
         add di,156
	       dec bx
	     jnz cube
	  jmp exit
	  
	  
	  Line:

         pop di
				   mov al,0x20
       mov ah,[shapeColor]
	       mov cx,4
		     line:
	         stosw
		       add di,158
		     loop line
	    jmp exit
	  
	  
	  LShape:
         
         ;printing
	       pop di 
		   mov al,0x20
       mov ah,[shapeColor]
	       mov cx,3
		     Lshape:
		       stosw
		       add di,158
		     loop Lshape
		  
		     sub di,158
		     stosw
	     jmp exit
	  
	  
	  JShape:
      
      ;printing
       pop di
		   mov al,0x20
       mov ah,[shapeColor]
	     mov cx,3
		   Jshape:
		     stosw
		     add di,158
		   loop Jshape
		  
		   sub di,162
		   stosw
     jmp exit

  restartY:
    cmp word [y],5
    jge skip2
       mov byte [flag],-1
    skip2:
    call RemoveLines
    mov word [PrevPos],-1
    mov ah,0
    int 1Ah
     ;shape Color
     and dx,0xF0
     mov byte[shapeColor],dl
       cmp dx,0x70
       jne skip4
         mov byte [shapeColor],0x20
       skip4:
       cmp dx,0xF0
       jne skip3
       mov byte [shapeColor],0x20
     skip3:

    mov ah,0
    int 1Ah
    and dx,3
    mov word [shapeNumber],dx


    mov word [x],32
    mov word [y],0		   

 exit:

ret 

clrscr:
  mov cx,2000
  xor di,di
  mov ax,0x0720
  rep stosw 

ret

PrintBackground:
  mov di,0
  mov bx,25
  mov ax,0x7020
  again:
   add di,38
   mov word [es:di],0x387c
   add di,2
   mov cx,25
   rep stosw
	 mov word [es:di],0x387c
   add di,2
	 mov cx,14
   rep stosw
   add di,40
  dec bx
  jnz again  
   
  mov di,734
  push word 7
  push word scoreStr
  call printstr
  mov di,1054
  push word 6
  push word timeStr
  call printstr
ret


endScreen:
  call clrscr

   mov di,2146
  mov cx, 9
  
  mov si, endStr  
  mov ah, 0x8B
  cld 
  nextchar1: 
  lodsb 
  stosw  
  loop nextchar1


ret

printstr:
  push bp
  mov bp, sp 
  mov cx, [bp+6]
  
  mov si, [bp+4]  
  mov ah, 0x70
  cld 
  nextchar: 
  lodsb 
  stosw  
  loop nextchar 
 pop bp 
ret 4


PrintTime:

 mov di,1070
 mov ax,[mins]
 add al,0x30
 mov ah,0x70
 stosw
 mov al,':'
 stosw
 cmp word [sec],10
 jae skipInPrintNum
 mov word [es:di],0x7030
 add di,2
 skipInPrintNum: 
 push word [sec]
 call printnum
ret 

printnum:
   push bp 
   mov bp, sp 
   push dx
   push bp
   push cx
   push ax
   push bx
   push di

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


 nextpos: pop dx ; remove a digit from the stack 
 mov dh, 0x70 ; use normal attribute 
 mov [es:di], dx ; print char on screen 
 add di, 2 ; move to next screen location 
 loop nextpos
  mov word [es:di],0x7020
 pop di
 pop bx
 pop ax
 pop cx
 pop bp
 pop dx 
 pop bp 
 ret 2 


Delay:
  push ax
  push bx
  push dx
  push cx
  mov ax,3
  count2:
  mov bx,0
  mov cx,0xFFFF
  count1:
  mov dx,2
  add bx,1
  loop count1
  dec ax
  jnz count2
  pop cx
  pop dx
  pop bx
  pop ax
ret

PrintScore:
 push di
  mov di,750
  push word [score]
  call printnum
 pop di
ret

start:
 mov ax,0xb800
 mov es,ax
call clrscr
call PrintBackground
call PrintScore

outerloop:

mov word [sec],59
move:
  call PrintTime
  call PrintShapes
    cmp byte [flag],0
    jl DisplayEndScreen
  call Delay 
  add word [y],1
  call PrintShapes
    cmp byte [flag],0
    jl DisplayEndScreen
  call Delay 
  add word [y],1
  call PrintShapes
    cmp byte [flag],0
    jl DisplayEndScreen
  call Delay
  add word [y],1
    call PrintShapes
    cmp byte [flag],0
    jl DisplayEndScreen
  call Delay
  add word [y],1

  dec word [sec]
  cmp word [sec],0
  jge move

dec word [mins]
cmp word [mins],0
jge outerloop

DisplayEndScreen:
call endScreen

;mov ah,0x1 ; special intrupt to not show the cursor
;int 0x21

mov ax,0x4c00
int 21h