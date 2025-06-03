[org 0x7c00]                  ; Bootloader loads at 0x7c00 in memory
KERNEL_LOCATION equ 0x7e00    ; Where kernel will be loaded (0x7c00 + 512)
SECTOR_COUNT equ 0x02    ; Where kernel will be loaded (0x7c00 + 512)

mov [BOOT_DISK], dl           ; Save boot disk number from BIOS in BOOT_DISK

xor ax, ax
mov es, ax                   ; Set ES = 0 (segment 0)
mov ds, ax                   ; Set DS = 0
mov bp, 0x8000               ; Set BP stack base (real mode)
mov sp, bp                   ; Set SP = BP (stack pointer)
mov bx, KERNEL_LOCATION      ; BX = address to load kernel

mov ah, 0x41               ; Check if LBA is supported
mov bx, 0x55AA             ; Magic number required by BIOS
int 0x13                   ; BIOS disk function

jc use_CHS                 ; If carry flag is set, LBA is not supported
jmp use_LBA                ; If LBA is supported, jump to LBA routine

use_CHS:
mov ah, 0x02               ; BIOS CHS disk read function
mov al, SECTOR_COUNT       ; Number of sectors to read
mov ch, 0x00               ; Cylinder 0
mov dh, 0x00               ; Head 0
mov cl, 0x02               ; Sector 2 (first sector is 1)
mov dl, [BOOT_DISK]        ; Boot disk number
int 0x13                   ; BIOS disk read
jnc no_error

mov bx, badcode_error_string_1
jmp loop_point_0

use_LBA:
mov ah, 0x42               ; BIOS LBA read function
mov dl, [BOOT_DISK]        ; Boot disk number
mov si, lba_packet         ; Address of LBA packet structure
int 0x13                   ; BIOS disk read
jnc no_error

mov bx, badcode_error_string_2
jmp loop_point_0

; i wanted to put data close to the code using it or some other reason im too lazy to scroll. Basically <INSERT_EXCUSE_HERE>
badcode_error_string_1:
    db "ERR: 0x1BCE", 0
; i wanted to put data close to the code using it or some other reason im too lazy to scroll. Basically <INSERT_EXCUSE_HERE>
badcode_error_string_2:
    db "ERR: 0x2BCE", 0
    
lba_packet:
    db 0x10                ; Packet size (16 bytes)
    db 0x00                ; Reserved
    dw SECTOR_COUNT        ; Number of sectors to read (using variable)
    dw KERNEL_LOCATION     ; Buffer location where kernel loads
    dw 0x0000              ; Segment (part of buffer address)
    dq 2                   ; LBA sector number (assuming kernel starts at sector 2)

loop_point_0:
    mov ah, 0x0E
    mov al, [bx]    ; char to print
    int 0x10

    inc bx
    cmp byte [bx], 0
    jne loop_point_0

jmp HALT_RM_B16


no_error:
mov ah, 0x00               ; BIOS set video mode function
mov al, 0x03               ; Mode 3 = 80x25 text mode
int 0x10                   ; BIOS video interrupt

CODE_SEG equ GDT_code - GDT_start
DATA_SEG equ GDT_data - GDT_start

cli                        ; Disable interrupts before GDT load
lgdt [GDT_descriptor]      ; Load Global Descriptor Table

mov eax, cr0               ; Read CR0 control register
or eax, 1                  ; Set PE bit (bit 0) to enable protected mode
mov cr0, eax               ; Write back to CR0

jmp CODE_SEG:start_protected_mode ; Far jump to reload CS and enter protected mode
jmp HALT_RM_B16            ; Infinite loop (should not reach here)

HALT_RM_B16:
    cli
    hlt
    jmp HALT_RM_B16

BOOT_DISK: db 0            ; Storage for boot disk number

; GDT definition for protected mode
GDT_start:
    GDT_null:
        dd 0x0
        dd 0x0

    GDT_code:
        dw 0xffff            ; Segment limit (max 64KB)
        dw 0x0               ; Base low
        db 0x0               ; Base middle
        db 0b10011010        ; Access byte (code segment, executable, readable, accessed)
        db 0b11001111        ; Flags (granularity, 32-bit)
        db 0x0               ; Base high

    GDT_data:
        dw 0xffff
        dw 0x0
        db 0x0
        db 0b10010010        ; Access byte (data segment, writable)
        db 0b11001111        ; Flags
        db 0x0

GDT_end:

GDT_descriptor:
    dw GDT_end - GDT_start - 1  ; Size of GDT - 1
    dd GDT_start                ; Address of GDT

[bits 32]
start_protected_mode:
    mov ax, DATA_SEG          ; Load data segment selector
	mov ds, ax
	mov ss, ax
	mov es, ax
	mov fs, ax
	mov gs, ax

	mov ebp, 0x90000		  ; Set 32-bit stack base pointer
	mov esp, ebp

    jmp KERNEL_LOCATION       ; Jump to kernel loaded at 0x7e00 (assumed to be 32-bit code)

HALT_PM_B32:
    cli
    hlt
    jmp HALT_PM_B32

times 510-($-$$) db 0         ; Pad boot sector to 510 bytes
dw 0xaa55                     ; Boot signature (must be 0xAA55)

; Debug output: print "ASPEN" on screen using VGA text buffer
mov byte [0xB8000], 'A'
mov byte [0xB8001], 0x0C     ; Color byte (red on black)
mov byte [0xB8002], 'S'
mov byte [0xB8003], 0x0C
mov byte [0xB8004], 'P'
mov byte [0xB8005], 0x0C
mov byte [0xB8006], 'E'
mov byte [0xB8007], 0x0C
mov byte [0xB8008], 'N'
mov byte [0xB8009], 0x0C

hlt                          ; Halt CPU here
jmp $                        ; Infinite loop to prevent running into unknown code
