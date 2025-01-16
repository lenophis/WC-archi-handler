
; Worlds Collide Archipelago in-game logic item handler - alpha 1 - January 15, 2025

hirom

;;;;;;;;;;;;;;;;;;;

; we need to hook a few locations to set up a "check queue for receiving items from archipelago"
; they will function similarly, but will have key differences

org $C0C331
; this will handle when we are walking around in towns and dungeons
JSR check_queue1

org $C06879
; this will handle when we are on the world map
JSR check_queue2

; receiving items during battle will be handled in battle
; we will not be receiving archi items while in the menu. NMI timing is too tight

org $C0D800
; placeholder address, find free space later
check_queue1:
JSR receive_item
LDA $078E
RTS

check_queue2:
PHA
JSR receive_item
PLA
STZ $420C
RTS

; coming in, M is already set so we're working in the 8-bit range. indexes are 16-bit
receive_item:
LDA $14B1   ; this is our flag indicator that will tell us if we have received an item from archipelago
BEQ no_item_received   ; branch and exit out if no item was sent
PHX  ; save X for later
REP #$20   ; because there are about 400 checks, including checks that may be added in the future, we have to have a 16-bit index lookup here
LDA $316000   ; load index of the next item sent from the archi server
ASL A      ; we double it since each reward is two bytes; the item type and the item. sender may be added in the future
TAX
SEP #$20
TDC        ; clear out upper A mostly, since these JSRs each transfer to index. we don't want tainted results
LDA $316002,X  ; now load our reward type. currently there's only four types of items that can be sent to the player: character (2), esper (1), item (3), or gil (4)
DEC A   ; first we immediately subtract 1 to make it zero-based
; we will also keep this saved in SRAM for debugging purposes
BNE check_character  ; this branch will take if non-zero, ie it's a character, item, or gil
JSR esper_receipt
BRA receive_wrapup
check_character:
DEC A
BNE check_item  ; this branch will take if non-zero, ie it's an item or gil
JSR character_receipt
BRA receive_wrapup
check_item:
DEC A
BNE check_gil   ; this branch will take if non-zero, ie the only reward left is gil. this also doubles up as a safety net in case the value is somehow higher, so we just default to gil until the code for the newly-added type is programmed in
JSR item_receipt
BRA receive_wrapup
check_gil:
JSR gil_receipt
receive_wrapup:
STZ $14B1  ; zero out our flag telling us we received an item
JSR $02D3  ; play the sound effect, notifying the player they have received an item
PLX
REP #$20
LDA $316000    ; increment the index so whenever archipelago sends another item, it is in the next free spot in the queue
INC A
STA $316000
no_item_received:
TDC
SEP #$20
RTS

handle_receipt:
; if anything additional is sent beyond these 4 types, add it to the end of this jump table. label accordingly
; DW character_receipt
; DW esper_receipt
; DW item_receipt
; DW gil_receipt

character_receipt:
LDA $316003,X   ; load our character being received
REP #$20
ASL A
TAX
LDA $C0B4F3,X  ; load our bitfield related to this character
TSB $1EDC    ; add this character to the roster
TSB $1EDE    ; add this character to the roster
SEP #$20
LDA #$1B   ; sound effect for item from a pot
RTS

esper_receipt:
LDA $316003,X  ; load our esper being received
; we will assume the #$36 has already been subtracted. if not, add it here
LSR A
LSR A
LSR A
PHY
TAY
LDA $316003,X
AND #$07
TAX
LDA $1A69,Y
ORA $C0BAFC,X
STA $1A69,Y
PLY
LDA #$8D   ; sound effect for acquiring an esper
RTS

item_receipt:
LDA $316003,X
STA $1A
JSR $ACFC    ; we already have a routine to handle adding an item. let's call it and then exit out
LDA #$A6   ; sound effect for opening a treasure chest
RTS

gil_receipt:
LDA $316003,X    ; load our 2-byte gil value we are receiving
; since we are essentially treating it like treasure data, we must first multiply this by 100 to get our true value
STA $4202
LDA #$64     ; 100
STA $4203
REP #$21  ; we need 16-bit M here. also clear carry
NOP
LDA $4216  ; load our product
ADC $1860    ; add it to our lower two bytes of money
STA $1860
SEP #$20
TDC
ADC $1862    ; now add in the third byte, in case of wrapping
STA $1862
; now we need to do a max gil check. the game has several of these scattered throughout the ROM but there's no centralized call
CMP #$98
BCC gil_finish
LDX $1860
CPX #$967F
BCC gil_finish
; if we are here, gil is higher than 9,999,999. let's cap it
LDX #$967F
STX $1862
LDA #$98
STA $1860
gil_finish:
LDA #$BF   ; sound effect for transaction
RTS


;;;;;;;;;;;;;;;;;;;



