org 0x8000
bits 16 

;Let's begin by going into graphic mode
call initGraphics

;Main image loop
imageLoop:
	; draw first frame
	mov cx, 0 ;player x to draw relative
	mov dx, 0 ;player z to draw relative
	mov di, image
	call drawEntity
    call copyBufferOver ;draw frame to screen
    call waitForNextFrame
	call resetBuffer ;reset screen to draw on empty canvas

	; draw second frame
	mov cx, 0 ;player x to draw relative
	mov dx, 0 ;player z to draw relative
	mov di, image1
	call drawEntity
    call copyBufferOver
    call waitForNextFrame
    call resetBuffer ;reset screen to draw on empty canvas

	; draw third frame
	mov cx, 0 ;player x to draw relative
	mov dx, 0 ;player z to draw relative
	mov di, image2
	call drawEntity
    call copyBufferOver ;draw frame to screen
	call waitForNextFrame
    call resetBuffer

    ; draw fourth frame
	mov cx, 0 ;player x to draw relative
	mov dx, 0 ;player z to draw relative
	mov di, image3
	call drawEntity
    call copyBufferOver ;draw frame to screen
	call waitForNextFrame
    call resetBuffer
	
jmp imageLoop
jmp $

waitForNextFrame:
	MOV     CX, 01H
	MOV     DX, 4240H
	MOV     AH, 86H
	INT     15H
	ret

;di = entity cx,dx = xpos,zpos
drawEntity:
	mov si, word [di]   ;get animation
	mov si, word [si+4] ;get first frame of animation
	mov ax, word [di+2] ;get entity x
	sub ax, cx          ;subtract the position of the player from the x position
	add ax, 80/2 - 50/2  ;relative to screen image drawing code for x position
	mov bx, word [di+4] ;get entity y
	sub bx, dx          ;subtract the position of the player from the z position
	add bx, 50/2 - 37/2 ;relative to screen image drawing code for z position
	call drawImage      ;draw image to buffer
	ret


%include "buffer.asm"

image:
    image_Anim  dw img          ;pointer to animation
    image_PosX  dw 0            ;position of image (x)
    image_PosZ  dw 0            ;position of image (z)
    ; image_AnimC dw 0

image1:
    image1_Anim  dw img1         ;pointer to animation
    image1_PosX  dw 0            ;position of image (x)
    image1_PosZ  dw 0            ;position of image (z)
    ; image1_AnimC dw 0

image2:
    image2_Anim  dw img2         ;pointer to animation
    image2_PosX  dw 0            ;position of image (x)
    image2_PosZ  dw 0            ;position of image (z)
    ; image2_AnimC dw 0

image3:
    image3_Anim  dw img3         ;pointer to animation
    image3_PosX  dw 0            ;position of image (x)
    image3_PosZ  dw 0            ;position of image (z)
    ; image2_AnimC dw 0
img:
	dw 1
	dw 1
	dw pika
	dw 0           ;zero end frame

img1:
	dw 1
	dw 1
	dw pika1
	dw 0

img2:
	dw 1
	dw 1
	dw pika2
	dw 0 	
img3:
	dw 1
	dw 1
	dw pika3
	dw 0 
pika incbin "bin/image.bin"
pika1 incbin "bin/image1.bin"
pika2 incbin "bin/image2.bin"
pika3 incbin "bin/image3.bin"

%assign usedMemory ($-$$)
%assign usableMemory (512*17)
%warning [usedMemory/usableMemory] Bytes used
times (512*17)-($-$$) db 0 ;kernel must have size multiple of 512 so let's pad it to the correct size

; nasm -fbin image.asm -o image.bin
; nasm -fbin bootloader.asm -o bootloader.bin
; cat bootloader.bin image.bin > bootableImage.bin
; qemu-system-i386 bootableImage.bin
