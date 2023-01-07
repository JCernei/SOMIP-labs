org 7C00h

 StartDate:  
         mov bx, 000Ah   ;Colour attribute (F = white)
         mov cx, 1       ;We will want to write 1 character
         mov dh, 0       ;X coordinate
         mov dl, 0       ;Y coordinate
         cld             ;Ensure direction flag is cleared (for LODSB)

         mov si, Date    ;Loads the address of the first byte of the message
                         
 .Char:  mov ah, 2       ;PC BIOS Interrupt 10 Subfunction 2 - Set cursor position
         int 10h
         lodsb           ;Load a byte of the message into AL.
                         
         mov ah, 0x0E       ;PC BIOS Interrupt 10 Subfunction 9 - Write character and colour
         int 10h
 
         inc dl          ;Advance cursor
         cmp al, 0       ;If we're not at end of message,
         jne .Char       ;continue loading characters


 StartName:  
         mov bx, 000Bh   ;Colour attribute (F = white)
         mov cx, 1       ;We will want to write 1 character
         mov dh, 11      ;X coordinate
         mov dl, 35      ;Y coordinate
         cld             ;Ensure direction flag is cleared (for LODSB)
         mov si, Name    ;Loads the address of the first byte of the message
 
 .Char:  mov ah, 2       ;PC BIOS Interrupt 10 Subfunction 2 - Set cursor position
         int 10h
         lodsb           ;Load a byte of the message into AL.
                         
         mov ah, 9       ;PC BIOS Interrupt 10 Subfunction 9 - Write character and colour
         int 10h
 
         inc dl          ;Advance cursor
        
         cmp al, 0       ;If we're not at end of message,
         jne .Char       ;continue loading characters
        

 StartGroup:  
         mov bx, 000Ch   ;Colour attribute (F = white)
         mov cx, 1       ;We will want to write 1 character
         mov dh, 24      ;X coordinate
         mov dl, 73      ;X coordinate
         cld             ;Ensure direction flag is cleared (for LODSB)
         mov si, Group   ;Loads the address of the first byte of the message
                         
 .Char:  mov ah, 2       ;PC BIOS Interrupt 10 Subfunction 2 - Set cursor position
         int 10h
         lodsb           ;Load a byte of the message into AL.
                         
         mov ah, 9       ;PC BIOS Interrupt 10 Subfunction 9 - Write character and colour
         int 10h
 
         inc dl          ;Advance cursor
         
         cmp al, 0       ;If we're not at end of message,
         jne .Char       ;continue loading characters
 

 Stop:   cli
         hlt
         jmp Stop


 Date:   db __?TIME?__, 0
 Name:   db "Cernei Ion", 0
 Group:  db "FAF-201", 0

 times 0200h - 2 - ($ - $$)  db 0    ;Zerofill up to 510 bytes
 dw 0AA55h       ;Boot Sector signature
