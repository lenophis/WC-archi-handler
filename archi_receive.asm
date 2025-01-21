
; Worlds Collide Archipelago in-game logic item handler - alpha 3 - January 20, 2025

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
LDA $1440   ; this is our flag indicator that will tell us if we have received an item from archipelago
BEQ no_item_received   ; branch and exit out if no item was sent
REP #$20
LDA $1F64   ; load the current map
CMP #$0033    ; is the current map WoB narshe mines for Moogle Defense?
SEP #$20
BNE proceed_item_receive  ; if not we don't care about the next check, move on and receive the item
LDA $1EA5
BIT #$40    ; is Moogle Defense ongoing or otherwise not done?
BEQ no_item_received   ; if this bit is CLEAR, we have to not acquire anything right now. character acquisition during Moogle Defense has known issues. it would be faster to hold the queue than to manually parse it.
proceed_item_receive:
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
STZ $1440  ; zero out our flag telling us we received an item
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
; TODO - call level averaging and natural abilities
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
STA $004202
LDA #$64     ; 100
STA $004203
REP #$21  ; we need 16-bit M here. also clear carry
NOP
LDA $004216  ; load our product
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

; now we need to handle battles
; we will have message pop-ups that occur during victory, similar to how objectives are handled. no sound effects will be needed, but senders may be added here

battle_item_receipt:
; this is separate from receiving items anywhere else. battles have their own array that gets copied back over to the live list after every fight
LDA $316003,X  ; load item being received
STA $2F35    ; the item received for message purposes
item_add:
; we're going to mostly copy the routine from bank C3, since it is more efficient than C1's
PHY
LDY $00     ; Y = 0
item_add_loop1:
; first we must loop through our existing items to see if what we're being sent is something we have already. if so, we add 1 to it
CMP $2686,Y    ; compare current item to the first slot in the battle array
BEQ item_match
CMP #$FF       ; is it no item? this is a failsafe. archi should never send nothing, but this will make sure that if it ever does that the game doesn't get stuck here
BEQ item_add_exit
INY
INY
INY
INY
INY
CPY #$0500     ; have we looped through the entire battle array?
BNE item_add_loop1
; at this point, we have determined that the item we are being sent is a new item. now we find the first free slot for it
LDY $00     ; Y = 0 again
item_add_loop2:
LDA $2686,Y
CMP #$FF    ; is it a blank space?
BEQ item_add_free_spot
INY
INY
INY
INY
INY
BRA item_add_loop2
item_add_free_spot:
LDA #$01
STA $2689,Y     ; we now have 1 of our new item
LDA $2F35    ; load our item again
STA $2686,Y     ; and add it to the battle array
LDA #$A6   ; sound effect for opening a treasure chest
PLY
RTS

item_match:
LDA $2689,Y
CMP #$63       ; do we have 99 of this item already?
BEQ item_add_exit   ; branch and exit if we do
INC A
STA $2689,Y
item_add_exit:
LDA #$A6   ; sound effect for opening a treasure chest
PLY
RTS


C0handle_item_battle:
; if we are here, we have at least 1 item to receive from archipelago. we're in battles, so we will process them by the messages that appear at the top of the screen
; since battles use different ram for some indicators, we must follow suit. items have their own battle array, for example.
PHY    ; let's save Y for now
REP #$30    ; let's make our indexes 16-bit again, as well as M
LDA $316000   ; load our queue index
ASL A
TAX
SEP #$20
TDC        ; clear out upper A mostly, since these JSRs each transfer to index. we don't want tainted results
LDA $316002,X  ; now load our reward type. currently there's only four types of items that can be sent to the player: character (2), esper (1), item (3), or gil (4)
DEC A   ; first we immediately subtract 1 to make it zero-based
; we will also keep this saved in SRAM for debugging purposes
BNE battle_check_character
LDA $316003,X   ; load our receiving esper
STA $2F35     ; store it for message purposes
JSR esper_receipt  ; we can use this pre-existing routine for espers
JSR $02D3     ; play the sound effect
LDA #$4B      ; placeholder. we need to add in a message for the esper
JSR battle_message  ; pop up our message at the top of the screen
BRA battle_receive_wrapup
battle_check_character:
DEC A
BNE battle_check_item
LDA $316003,X   ; load our receiving character
STA $2F35     ; store it for message purposes
JSR character_receipt
JSR $02D3     ; play the sound effect
LDA #$4A      ; placeholder. we need to add in a message for the character
JSR battle_message  ; pop up our message at the top of the screen
BRA battle_receive_wrapup
battle_check_item:
DEC A
BNE battle_check_gil
JSR battle_item_receipt
JSR $02D3     ; play the sound effect
LDA #$20
JSR battle_message  ; pop up our message at the top of the screen
BRA battle_receive_wrapup
battle_check_gil:
JSR gil_receipt     ; add our gil
JSR $02D3     ; play the sound effect
REP #$21
LDA $004216      ; load our gil product
STA $2F38      ; save it for the message parameter. otherwise it'll just show 0
SEP #$20
LDA #$26
JSR battle_message  ; pop up our message at the top of the screen
battle_receive_wrapup:
STZ $1440  ; zero out our flag telling us we received an item
REP #$21       ; 
LDA $316000    ; increment the index so whenever archipelago sends another item, it is in the next free spot in the queue
INC A
STA $316000
TDC
SEP #$30      ; restore index back to 8-bit
PLY
RTL

battle_message:
; now we must call a message so it can pop up. the issue is the setup is back in bank C2. we now have to recreate the setup here
; this will be the equivilant of C2/5FD4
C25FD4:	PHP
C25FD5:	SEP #$20
C25FD7:	CMP #$FF
C25FD9:	BEQ C25FED    ; using the address as the label, essentially branch and exit if the message is null
C25FDB:	STA $2D6F
C25FDE:	LDA #$02
C25FE0:	STA $2D6E
C25FE3:	LDA #$FF
C25FE5:	STA $2D72
C25FE8:	LDA #$04
; this is where the JSR $6411 would go. we'll just copy that small routine here instead since nothing else will be calling it
C26411:	PHX
C26412:	PHY
C26413:	PHP
C26414:	SEP #$20
C26416:	REP #$11    ; indexes are 16-bit, also clear carry
C26418:	PHA
C26419:	TDC
C2641A:	PLA
C2641B:	CMP #$02
C2641D:	BNE C26425   ; skip straight to the JSL
C2641F:	LDA $B1
C26421:	BMI C26429   ; branch and exit out
C26423:	LDA #$02
C26425:	JSL $C10000
C26429:	PLP
C2642A:	PLY
C2642B:	PLX
; this is where the RTS from $6411 would go. we're done here
C25FED:	PLP
C25FEE:	RTS

org $C25F78
; this is after the loop of items found from enemies, after money dropped from enemies (and subsequently doubled from Cat Hood), but before the money message. the money from monsters will remain the final message
JSR archi_battle

org $C266E5
; this is tentative free space. find other space if necessary
archi_battle:
LDA $1440    ; load flag indicator telling us if we have received an item from Archipelago
BEQ battle_no_item_received
REP #$20
LDA $1F64   ; load the current map
CMP #$0033    ; is the current map WoB narshe mines for Moogle Defense?
SEP #$20
BNE proceed_battle_receive  ; if not we don't care about the next check, move on and receive the item
LDA $1EA5
BIT #$40    ; is Moogle Defense ongoing or otherwise not done?
BEQ battle_no_item_received   ; if this bit is CLEAR, we have to not acquire anything right now. character acquisition during Moogle Defense has known issues. it would be faster to hold the queue than to manually parse it.
proceed_battle_receive:
JSL C0handle_item_battle
battle_no_item_received:
LDA $2F3E    ; load our first byte of money dropped by monsters
RTS


;;;;;;;;;;;;;;;;;;;

; now we need to make a couple changes/additions to bank C1 to account for the two new messages, since they will be parsing parameters that need to be output correctly

org $C15EED
JMP (new_table,X)    ; this jump table has 4 entries, and we need to add 2 more to it. there isn't space where this table is in the original game, so we have to move it to do so

org $C168AC
; this is where the ATB table is in the original game. we will be moving it to bank F0.
new_table:
DW $5F40
DW $5F06     ; item names are handled with this one
DW $5F00
DW $5F13
DW handle_character_name
DW handle_esper_name

handle_character_name:
LDA $2F35    ; load our character ID
XBA
LDA #$06
STA $616D
REP #$20
STA $004202   ; store ID and 6 as the multipliers. each character name is 6 letters in length
NOP
NOP     ; waste a few cycles
TDC
LDA $004216   ; load product
TAX
SEP #$20
handle_character_loop:
LDA $C478C0,X    ; load character name
CMP #$FF     ; is it a null character?
BEQ handle_character_exit    ; branch if so, we're done here
JSR $6111
INX
DEC $616D
BNE handle_character_loop
handle_character_exit:
RTS

handle_esper_name:
LDA $2F35    ; load our esper ID
XBA
LDA #$08
STA $616D
REP #$20
STA $004202   ; store ID and 8 as the multipliers. each esper name is 8 letters in length
NOP
NOP     ; waste a few cycles
TDC
LDA $004216   ; load product
TAX
SEP #$20
handle_esper_loop:
LDA $E6F6E1,X    ; load esper name
CMP #$FF     ; is it a null character?
BEQ handle_esper_exit    ; branch if so, we're done here
JSR $6111
INX
DEC $616D
BNE handle_esper_loop
handle_esper_exit:
RTS

; as of now, there is 0x1E free bytes of space left over from the ATB takeover

; now we need to adjust the look-up table for the ATB, since we're moving it to bank F0
org $C16861
LDA $F06500,X    ; new tentative location for the ATB table

org $F06500
DB $F1,$F0,$F0,$F0
DB $F2,$F0,$F0,$F0
DB $F3,$F0,$F0,$F0
DB $F4,$F0,$F0,$F0
DB $F5,$F0,$F0,$F0
DB $F6,$F0,$F0,$F0
DB $F7,$F0,$F0,$F0
DB $F8,$F0,$F0,$F0
DB $F8,$F1,$F0,$F0
DB $F8,$F2,$F0,$F0
DB $F8,$F3,$F0,$F0
DB $F8,$F4,$F0,$F0
DB $F8,$F5,$F0,$F0
DB $F8,$F6,$F0,$F0
DB $F8,$F7,$F0,$F0
DB $F8,$F8,$F0,$F0
DB $F8,$F8,$F1,$F0
DB $F8,$F8,$F2,$F0
DB $F8,$F8,$F3,$F0
DB $F8,$F8,$F4,$F0
DB $F8,$F8,$F5,$F0
DB $F8,$F8,$F6,$F0
DB $F8,$F8,$F7,$F0
DB $F8,$F8,$F8,$F0
DB $F8,$F8,$F8,$F1
DB $F8,$F8,$F8,$F2
DB $F8,$F8,$F8,$F3
DB $F8,$F8,$F8,$F4
DB $F8,$F8,$F8,$F5
DB $F8,$F8,$F8,$F6
DB $F8,$F8,$F8,$F7
DB $F8,$F8,$F8,$F8


;;;;;;;;;;;;;;;;;;;

; now that the receiving end of things has been taken care of, we need to do initialization of the queue

org $C37064
JMP blank_more_SRAM

org $C0FFD8    ; we are here only to change the internal ROM header to indicate a larger SRAM size
DB $04

org $C0BDE4
; we are hijacking the part of SRAM init that sets SwdTech names to SRAM. this feature is never used outside of the Japanese version
; coming in, X is 0
STX $1440  ; zero our archipelago flag
NOP    ; this removes the rest of the LDA $CF3C40,X instruction
NOP
NOP
NOP    ; this removes the STA $1CF8,X instruction
NOP    ; this removes the INX instruction
NOP
NOP
NOP    ; this removes the CPX #$0030 instruction
NOP
NOP    ; this removes the BNE instruction for the loop

org $C3FB22    ; this is the first available byte free in bank C3. move accordingly if necessary
blank_more_SRAM:
; right now, the added SRAM use will be:
; first two bytes are the item queue index
; the next 0x320 bytes are the items being sent from the Archipelago server
; repeat this 3 more times. the first copy will be the "live" copy, while the next three will be for each save slot respectively
SEP #$20
LDA #$31
PHA
PLB       ; now we're switching to bank 31
LDX #$0000
REP #$20
blank_more_loop:
STZ $6000,X     ; we're going to be blanking $316000 through $317FFF. we won't be using nearly this much, but we're blanking it all anyway in case future use needs it
INX
INX
STZ $6000,X
INX
INX
CPX #$2000
BNE blank_more_loop
SEP #$20
PLB
RTS

;;;;;;;;;;;;;;;;;;;

; now we need to handle saving the queue, since players will inevitably not be playing at all times

SRAM_slot:
DW $0000     ; slot 1
DW $0400     ; slot 2
DW $0800     ; slot 3

SRAM_save:
; we will put off writing the SRAM markers until we are done saving the queue
LDA $66    ; load our save slot indicator again
ASL A
TAX
REP #$20
LDA SRAM_slot,X
TAX
SEP #$20
LDY $00
SRAM_save_loop:
LDA $316000,X    ; load current queue byte
STA $316400,X    ; save it to this slot's queue
INY
INX
CPX #$0400
BNE SRAM_save_loop
JMP $7083       ; save the SRAM markers

SRAM_load:
; upon loading a save, we must transfer that save's queue to the live queue
LDA $66    ; load our slot indicator again
ASL A
TAX
REP #$20
LDA SRAM_slot,X
TAX
SEP #$20
LDY $00
SRAM_load_loop:
LDA $316400,X     ; load slot's queue byte
STA $316000,X     ; copy it to the live queue
INY
INX
CPX #$0400
BNE SRAM_load_loop
RTS

; the only issue currently is that if you back out and start a new game, the "live" queue is from whatever slot you last wanted to load
; so we have to account for that
restore_live_SRAM:
JSR blank_more_SRAM   ; this will be called if you cancel out of loading a save slot
JMP $18C1

SRAM_load2:
JSR SRAM_load
JMP $6915      ; execute the next routine like we were originally going to

org $C329EE
JSR SRAM_load2     ; this will load up the slot of whatever you confirm on the load screen

