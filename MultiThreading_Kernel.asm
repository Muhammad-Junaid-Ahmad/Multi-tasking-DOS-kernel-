;;;;;;;;;;;;;;;;;;;;;;;;
;Int 21 Service 20 ------  AX <- subsevice
;Sub service 00 = Add thread 			:::::  DI contains IP of thread CS is pushed in interupt call
;Sub service 01 = Remove thread  		::::::: current running thread will be removed from sequence
;Sub service 02	= Suspend/Pause thread	:::::::	number of thread given in bx will be suspended
;Sub service 03	= Resume thread			:::::::	number of thread given in bx will be resumed

;Timer Interupt : Changing Process

org 100h
jmp start

defaultINT21: dd 0

;first two words empty for later use
;PCB layout:
;		   00|01	02		04		06		08		10		12		14		16		18		20		22		24		26		28		30
;		 next|prev	suspend AXSave, BXSave, CXSave, DXSave, SISave, DISave, BPSave, SPSave, SSSave, IPSave, CSSave, DSSave, ESSave, FLAGSave
PCB:  	 dw 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000
times 15 dw 0xFFFF, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000

STACK: times 512*16 db 0
Acode: db '0123456789ABCDEF'

;;if Suspend = 0 then task will run otherwise it is suspended
Suspend EQU 02
AXSave EQU 04
BXSave EQU 06
CXSave EQU 08
DXSave EQU 10
SISave EQU 12
DISave EQU 14
BPSave EQU 16
SPSave EQU 18
SSSave EQU 20
IPSave EQU 22
CSSave EQU 24
DSSave EQU 26
ESSave EQU 28
FLSave EQU 30

curproc: dw 0	;holds the number of the current process being run when interupt happens

;;;;;;;;;;;;;;;;;;;;;;;;;;;;	Hook For the TIMER 	;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Changing process
CHNG_Proc:

call SaveState
call PrintRegs		;printing all registers of current task
push word[CS:curproc]
call GetNext
mov byte[CS:curproc],al
call PrintRegs		;printing all registers of next task
call RestoreState

push ax
mov al,0x20
out 0x20,al
pop ax

iret

;Saving current state registers
SaveState:
push bx
mov bx,[CS:curproc]

shl bx,5

mov [CS:PCB+bx+AXSave],ax
pop ax		;getting pushed bx into ax
mov [CS:PCB+bx+BXSave],ax
mov [CS:PCB+bx+CXSave],cx
mov [CS:PCB+bx+DXSave],dx
mov [CS:PCB+bx+SISave],si
mov [CS:PCB+bx+DISave],di
mov [CS:PCB+bx+BPSave],bp
mov [CS:PCB+bx+SSSave],ss
mov [CS:PCB+bx+DSSave],ds
mov [CS:PCB+bx+ESSave],es


pop si		;getting return address into si to free IP,CS,Flags
pop ax
mov word[CS:PCB+bx+IPSave],ax
pop ax
mov word[CS:PCB+bx+CSSave],ax
pop ax
mov word[CS:PCB+bx+FLSave],ax

;now storing sp because it is at its original place before interupt happened
mov word[CS:PCB+bx+SPSave],sp

push si 	;pushing return address back onto stack

ret


;Getting the number of next process to retrieve state
;next/prev process of parameter will be returned 	ah = prev | al = next
GetNext:
push bp
mov bp,sp
push bx
push cx
mov bx,[bp+4]
shl bx,5

findnext:

mov ax,[CS:PCB+bx]	;returning number of next/prev process in AX
mov bx,[CS:PCB+bx]
xor bh,bh
shl bx,5
mov cx,[CS:PCB+bx+Suspend]	;checking if this task is suspended or not
cmp cx,0x0000
jne findnext
;ah = prev process	al= next process
found:
pop cx
pop bx
pop bp
ret 2


;Restoring the registers for next process
RestoreState:
mov bx,[CS:curproc]

shl bx,5

pop si
mov ss,[CS:PCB+bx+SSSave]		;restoring SS
mov sp,[CS:PCB+bx+SPSave]		;restoring SP
push word[CS:PCB+bx+FLSave]			
push word[CS:PCB+bx+CSSave]			;pushing Flags,IP,CS so that Iret will load these
push word[CS:PCB+bx+IPSave]
push si							;pushing the return address

mov ax,[CS:PCB+bx+AXSave]
mov cx,[CS:PCB+bx+CXSave]
mov dx,[CS:PCB+bx+DXSave]		;restoring all general purpose registers
mov si,[CS:PCB+bx+SISave]
mov di,[CS:PCB+bx+DISave]
mov bp,[CS:PCB+bx+BPSave]
mov ds,[CS:PCB+bx+DSSave]
mov es,[CS:PCB+bx+ESSave]

push word[CS:PCB+bx+BXSave]
pop bx

ret

;;;;;;;;;;;;;;;		TIMER INTERUPT end		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

msg: db 'No Free PCB'
sz: dw 11  
;;;;;;;;;;;;;;		INT 21h , chaining	;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Service 20 = Add / Remove Thread
;Sub Service 01 = Remove Thread  || Sub Service 00 Add Thread

;AH will hold Service	AL wull hold SubService
;DI will hold IP of task to be multitasked
;CS will be get from stack
I21_20:

cmp ax,2000h
jne NoAdd
call Add_Thread
cmp word[CS:FPCB],15
jna NP
	
	mov ax,0xB800
	mov es,ax
	mov ah,0x87
	mov bx,3910		;location of screen
	mov si, msg		;adress of string
	mov cx,[CS:sz]		;lenght of string
	stringtrav:
		mov al,[CS:si]
		mov [es:bx],ax
		inc si
		add bx,2
	loop stringtrav

NP: 
iret

NoAdd:
cmp ax,2001h
jne NoRemove
call Remove_CurThread
NoRemove:

cmp ax,2002h
jne NoSuspend
call SuspendThread

NoSuspend:

cmp ax,2003h
jne NoResume
call ResumeThread

NoResume:

cmp ax,4c00h
jne default
	call KillAllThreads
default:
jmp far [CS:defaultINT21]
;;iret


;variable to hold the free PCB number found
FPCB: dw 0
;;Adding a new thread to PCB on which int 21 service 20 is called
Add_Thread:
;;Finding free PCB is available
mov bx,0
mov word[CS:FPCB],0

nextPCB:
	add bx,32
	inc word[CS:FPCB]
	cmp bx,32*16
	jnae skip
		mov word[CS:FPCB],0x00FF
		jmp exit
	skip:
	cmp word[CS:PCB+bx],0xFFFF
	jne nextPCB
	
mov word[CS:PCB+bx+DSSave],ds
mov word[CS:PCB+bx+ESSave],es

mov word[CS:PCB+bx+AXSave],0
mov word[CS:PCB+bx+BXSave],0
mov word[CS:PCB+bx+CXSave],0
mov word[CS:PCB+bx+DXSave],0
mov word[CS:PCB+bx+SISave],0
mov word[CS:PCB+bx+DISave],0
mov word[CS:PCB+bx+BPSave],0
mov word[CS:PCB+bx+FLSave],0x7200
mov si,[CS:FPCB]
shl si,9			; si*512 
add si, 512+STACK		; si <- si*512 + STACK
mov word[CS:PCB+bx+SSSave],CS

sub si,2
mov word[CS:si],CS		;pushing IP:CS for ending of task
sub si,2						
mov word[CS:si],Remove_CurThread	;These IP:CS if user uses retf instead of calling kill thread service

mov word[CS:PCB+bx+SPSave],si

pop si 		;return address
pop ax		;IP before interupt
pop bp		;CS before interupt

mov word[CS:PCB+bx+IPSave],di		;IP of function to be multitasked
mov word[CS:PCB+bx+CSSave],bp		;CS of program

push bp
push ax
push si


mov bx,[CS:FPCB]		;changing curproc from 0 to the PCB number where new thread is placed

;;Changing cur.PCB.next -> PCB0.next
mov ax,[CS:PCB]			;picking next of PCB 0
shl bx,5
xor ah,ah
mov word[CS:PCB+bx],ax		;making next of cur PCB = next of PCB 0


;;Changing PCB0.next.prev -> cur.PCB
xor bx,bx
mov bl,al
shl bx,5				;changing prev of PCB0's next to cur PCB
mov ax,[CS:FPCB]
mov byte[CS:PCB+bx+1],al

;;Changing PCB0.next -> cur.PCB
mov byte[CS:PCB],al	;making next of PCB 0 = cur PCB

exit:
ret


ProTORemove: dw 0
;;Removing the current process from PCB on which Int 21 service 4C is called
Remove_CurThread:
mov bx,[CS:curproc]
mov word[CS:ProTORemove],bx

push word[CS:curproc]
call GetNext
mov byte[CS:curproc],al
call RestoreState

push bx		;saving bx for changed process
push ax
mov bx,[CS:ProTORemove]
shl bx,5
mov ax,[CS:PCB+bx]

cmp word[CS:ProTORemove],0
je no
mov word[CS:PCB+bx],0xFFFF		;making prev/next = FF/FF -> signal for Free PCB
no:
xor bx,bx
mov bl,ah
shl bx,5					;putting next of this in its prev.next
mov byte[CS:PCB+bx],al

xor bx,bx
mov bl,al
shl bx,5					;putting prev of this in its next.prev
mov byte[CS:PCB+bx+1],ah

push word[CS:curproc]
push word[CS:ProTORemove]
pop word[CS:curproc]
call PrintRegs
pop word[CS:curproc]

pop ax
pop bx
iret

;;Int 21 Service 20 Subservice 2
;; bx will hold the number of taks i.e PCB to suspend
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SuspendThread:
shl bx,5
mov word[CS:PCB+bx+Suspend],0x000F
pop bx
iret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Int 21 Service 20 Subservice 3
;; bx will hold the number of taks i.e PCB to resume
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ResumeThread:
shl bx,5
mov word[CS:PCB+bx+Suspend],0x0000
pop bx
iret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
KillAllThreads:
cmp word[CS:curproc],0
je NoRestore
	mov word[CS:curproc],0
	call RestoreState
NoRestore:
;saving the registers to be used
push cx
push bx
xor bx,bx
mov word[CS:PCB],0x0000
mov cx,15
RNT:
	add bx,32
	mov word[CS:PCB+bx],0xFFFF
	loop RNT
pop bx
pop cx
ret


;;;;;;;;;;		END OF CHAINED INT 21h		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;Sub Routine to print all reg of cur proc;;;;;;;;
PrintRegs:
push di
push si
push ax
push es
push bx
push cx

mov bx,[CS:curproc]
mov di,3994
call PrintNum

jmp skip1
ACode: db '0123456789ABCDEF'
skip1:

mov ax,0xb800
mov es,ax		;moving ES to video memory
mov ah,0x07		;setting attribute for words

mov si,[CS:curproc]
shl si,5
mov cx,16
;Pushing every word onto stack
numtrav:
	mov di,4
	mov bx,[CS:PCB+si]
	wordpush:			;spliting word into 4 bit parts and converting them into Hex and pushing
		push bx
		and bx,0x000F
		mov al,[CS:ACode+bx]
		pop bx
		push ax
		shr bx,4
		dec di
	jnz wordpush
	add si,2
loop numtrav

mov cx,16
mov bx,300
mov ax,[CS:curproc]
shl ax,4

sub bx,ax
nextnum:								
	mov di,4		;poping every word and placing into video memory
	popword:
		pop ax
		mov [es:bx],ax
		add bx,2
		dec di
	jnz popword
	mov word[es:bx],0x0720		;placing a space after every word
	add bx,152
loop nextnum

pop cx
pop bx
pop es
pop ax
pop si
pop di
ret 



PrintNum:
;; bx contains num
;;di contains screen loc ending
;;just printing number of current proc
push ax
push bx
push cx
push es

push 0xb800
pop es

mov ah,0x07
mov cx,4
nextnum1:
	push bx
	and bx,0x000F
	mov al,[CS:Acode+bx]
	mov [es:di],ax
	pop bx
	shr bx,4
	sub di,2
	loop nextnum1


pop es
pop cx
pop bx
pop ax

ret 


;;;;;;;;;;		Hooking mechansim		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
start:
xor ax,ax
mov es,ax


mov ax,[ES:0x21*4]
mov word[CS:defaultINT21],ax
mov ax,[ES:0x21*4+2]
mov word[CS:defaultINT21+2],ax

mov word[ES:0x21*4],I21_20
mov word[ES:0x21*4+2],CS

cli
mov word[ES:8*4],CHNG_Proc
mov word[ES:8*4+2],CS

;;Making these hooks a TSR
mov dx,start
add dx,15
shr dx,4
mov ax,3100h
int 21h