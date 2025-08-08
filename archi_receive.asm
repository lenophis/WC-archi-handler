
; Worlds Collide Archipelago in-game logic item handler - alpha 5 - August 8, 2025
; Worlds Collide Archipelago item gift logic handler - alpha 1 - August 8, 2025

hirom

table FF6-FWF.tbl,rtl

;;;;;;;;;;;;;;;;;;;

; we need to hook one of our unused menu calls to now call a menu screen to send gifts for multiworld users

org $C300F0
DW C3send_gift


org $C30277
; we are hooking the massive LUT that the main menu loop calls
DW C3send_how_many_initiate     ; #$4E
DW C3send_how_many_sustain      ; #$4F

org $C3029B
; we are hooking the massive LUT that the main menu loop calls
DW C3send_item_list_sustain    ; #$60

org $C302CB
; we are hooking the massive LUT that the main menu loop calls
DW C3send_item_list_initiate    ; #$78
DW $9E7D     ; switching characters while selecting a relic slot to fill; #$79
DW $9E8B     ; switching characters while selecting a relic slot to empty; #$7A
DW C3send_gift_initiate    ; #$7B
DW C3send_gift_sustain    ; #$7C

; we need to hook a few locations to set up a "check queue for receiving items from archipelago"
; they will function similarly, but will have key differences

org $C0C331
; this will handle when we are walking around in towns and dungeons
JSR check_queue1

org $C06879
; this will handle when we are on the world map
JSR check_queue2

org $C09926
; hook our event script LUT to add in a command. this will parse the quantity of the item the player is gifting to another, and prep it for dialogue display
DW command_76    ; event command #$66 - process a number fed with the command and prepare it for dialogue display. can handle 8, 16, or 24 bit values
DW command_67    ; event command #$67 - process all incoming gifts from the multiworld server

org $C099A2
; hook our event script LUT to add in a command so we can call our menu
DW C0multi_gift_item  ; event command #$A4

org $C09A26
; hook our event script LUT to add in a command to physically send out a gift to another player. the SNI client will then grab this and parse as necessary
DW C0multi_gift_send  ; event command #$E6


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
JSR receive_item_world
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
; TODO - add in the routine call for level averaging here if the flag is set
JSR $A17F     ; do natural abilities for this character
LDA #$1B   ; sound effect for item from a pot
BRA receive_wrapup
check_item:
DEC A
BNE check_gil   ; this branch will take if non-zero, ie the only reward left is gil. this also doubles up as a safety net in case the value is somehow higher, so we just default to gil until the code for the newly-added type is programmed in
JSR item_receipt
BRA receive_wrapup
check_gil:
JSR gil_receipt
receive_wrapup:
DEC $1440     ; do not zero this out in the event we still have items to process
; STZ $1440  ; zero out our flag telling us we received an item
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

receive_item_world:
LDA $1440   ; this is our flag indicator that will tell us if we have received an item from archipelago
BEQ no_item_received_world   ; branch and exit out if no item was sent
REP #$20
LDA $1F64   ; load the current map
CMP #$0033    ; is the current map WoB narshe mines for Moogle Defense?
SEP #$20
BNE proceed_item_receive_world  ; if not we don't care about the next check, move on and receive the item
LDA $1EA5
BIT #$40    ; is Moogle Defense ongoing or otherwise not done?
BEQ no_item_received_world   ; if this bit is CLEAR, we have to not acquire anything right now. character acquisition during Moogle Defense has known issues. it would be faster to hold the queue than to manually parse it.
proceed_item_receive_world:
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
BNE check_character_world  ; this branch will take if non-zero, ie it's a character, item, or gil
JSR esper_receipt
BRA receive_wrapup_world
check_character_world:
DEC A
BNE check_item_world  ; this branch will take if non-zero, ie it's an item or gil
JSR character_receipt
; TODO - add in the routine call for level averaging here if the flag is set
JSR $A17F     ; do natural abilities for this character
LDA #$1B   ; sound effect for item from a pot
BRA receive_wrapup_world
check_item_world:
DEC A
BNE check_gil_world   ; this branch will take if non-zero, ie the only reward left is gil. this also doubles up as a safety net in case the value is somehow higher, so we just default to gil until the code for the newly-added type is programmed in
JSR item_receipt
BRA receive_wrapup_world
check_gil_world:
JSR gil_receipt
receive_wrapup_world:
DEC $1440
; STZ $1440  ; zero out our flag telling us we received an item
STA $2141    ; play the sound effect
LDA #$80     ; normal speaker balance
STA $2142
LDA #$18
STA $2140    ; APU I/O register 0
PLX
REP #$20
LDA $316000    ; increment the index so whenever archipelago sends another item, it is in the next free spot in the queue
INC A
STA $316000
no_item_received_world:
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
; TODO - add the routine call to level average here
JSR $A17F     ; do natural abilities for this character
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
DEC $1440
; STZ $1440  ; zero out our flag telling us we received an item
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

C0multi_gift_item:
; now we need to handle the setup process to send out items as gifts to other players in the multiworld. this is different than typical sending/receiving like would happen with checks
LDA #$05
STA $0200   ; which menu aspect we are calling. in this case the world recipient and subsequent item list
LDA #$FF
STA $0201   ; anti-corruption purposes. this is the player
STA $0205   ; anti-corruption purposes. this is the item
JSR $C6CA   ; call the menu
LDA $0205   ; load item to send
CMP #$FF    ; did we select an item?
BEQ C0multi_no_item   ; branch if not
; with either of these two checks failing, we will branch over our script of sending an item out. essentially exit out
LDA $0201   ; load recipient
CMP #$FF    ; do we have a valid recipient?
BEQ C0multi_no_item   ; branch if not
LDA #$02
TRB $1EAF
DEC A
STA $84     ; for some reason, $84 must be non-zero (or 1) when exiting a menu call to $C6CA
JMP $9B70   ; advance the queue 1 byte

C0multi_no_item:
; if we are here, we need to skip over the next parts of the script
; it will be a message call with a prompt to verify if that's the item you want to send to that player
LDA #$02
TSB $1EAF
DEC A
STA $84     ; for some reason, $84 must be non-zero (or 1) when exiting a menu call to $C6CA
JMP $9B70   ; advance the queue 1 byte

C0multi_gift_send:
; do code here for SNI purposes
; first, remove the item from your inventory
LDX $00
C0multi_gift_loop:
LDA $1869,X   ; load inventory slot
CMP $0205    ; compare it with the item we are sending out
BEQ C0remove_item1
INX
CPX #$0100   ; have we looped through the entire inventory yet?
BNE C0multi_gift_loop
BRA C0multi_finish    ; if for whatever reason our previous failsafe didn't work, exit and attempt to send nothing

C0remove_item1:
LDA $1969,X
SEC
SBC $0202
STA $1969,X
BNE C0multi_item_quantity
LDA #$FF
STA $1869,X    ; this item slot is now empty
C0multi_item_quantity:

C0multi_finish:
LDA #$01
JMP $9B70    ; advance the event queue one byte


C0send_check_characters:
; we need to add in a check for a character decoder
CMP #$1C    ; is this our player tag?
BEQ C0multi_decode_player   ; branch if so. we need to parse it for the dialogue
CMP #$02
BCC C0multi_decode_exit     ; is it 0 or 1? ie terminator or new line?
CMP #$10
BCS C0multi_decode_exit
C0multi_decode_player:
TDC
LDA $CF
BPL C0multi_decode_exit    ; branch if $CF is positive. don't do this now
LDA $0201    ; load our recipient
REP #$20
ASL A
ASL A
ASL A
ASL A      ; * 16
TAY
TDC
SEP #$20
LDA #$F1
PHA
PLB
C0multi_decode_loop:
LDA $0002,Y    ; load from F1/0002 indexed
CMP #$FF      ; space or terminator?
BEQ C0multi_decode_exit
SEC
SBC #$60      ; subtract to set to VWF values
TAX
LDA $C48FC0,X  ; load width for variable font cell
CLC
ADC $C0
STA $C0
INY
CPY #$0010
BEQ C0multi_decode_exit
BRA C0multi_decode_loop

C0multi_decode_exit:
LDA #$00
PHA
PLB
SEC
RTS

C0multi_player_render:
; now we need to actually grab the player name and draw it to the screen
LDA $0201    ; load our recipient
REP #$20
ASL A
ASL A
ASL A
ASL A      ; * 16
TAY
SEP #$20
TDC
LDX $00     ; X = #$0000
LDA #$F1
PHA
PLB
C0multi_player_loop:
LDA $0002,Y    ; $F1/0002 indexed
SEC
SBC #$60
STA $7E9183,X
CMP #$9F
BEQ C0multi_player_finish
INY
INX
CPY #$0010
BEQ C0multi_player_finish
BRA C0multi_player_loop
C0multi_player_finish:
TDC
PHA
PLB
STA $7E9183,X
STZ $CF
JMP $8263



;;;;;;;;;;;;;;;;;;;;
; event command #$66 format: #$66 address 00/01/02 => last byte selects 8/16/24 bit mode
; feed the command a 2-byte argument for the number to display, so 66 xx xx yy
command_76:
LDA #$7E
STA $24        ; bank byte is #$7E
PEI ($EB)      ; !
REP #$20
PLA            ; !
STA $22        ; address to look into is now 7E/xxxx
LDA [$22]      ; load the contents of 7E/xxxx
PHA            ; save it for later, eventually for number displaying
LDA $ED        ; 8/16/24-bit marker, $EE is garbage anyway so there's no need to mask it out
LSR A
BCS sixteen_bits
LSR A
BCS twentyfour_bits
PLA            ; get our number to display
STA $22        ; save it
STZ $23        ; except this is 8-bit mode, so kill that middle byte (and high byte)
BRA finish_up
sixteen_bits:
PLA            ; get our number to display
STA $22        ; save it
SEP #$20
STZ $24        ; this must be kept clean in 16-bit mode, otherwise the number will be at least 8.2 million
finish_up:
TDC
JSR $02E5      ; display contents of $22 to dialogue, the only question mark is if $26 matters at all, and indications are it does not
LDA #$04
JMP $9B5C

twentyfour_bits:
INC $22        ; coming in, A is 16-bit. It has to be, otherwise there's a wrapping error (^_^)
               ; but wait, $24 isn't accounted for! true, but we don't care at all about bank 7F
INC $22        ; increment up 2 bytes to get to the highest byte
LDA [$22]      ; load 7E/xxxx+2
STA $24        ; save highest byte, used for number display
REP #$20
PLA
STA $22        ; and finally save the first two for number display
BRA finish_up

command_67:
; event command #$67 format is just the byte. it will handle all items being received
LDX $00    ; zero index
TXY        ; and also to Y for the inventory index
REP #$20
LDA $316C00    ; load gift index queue
BEQ no_gift_received
DEC A
ASL A
TAX
gift_item_queue_loop:
REP #$20
LDA $316C02,X    ; load the first item received
BEQ out_of_gifts   ; sanity check to make sure we are actually receiving an item, or if we've looped through our queue and need to exit out
STA $1A
SEP #$20
LDA #$02
TSB $1EAF      ; our marker to indicate we have received and parsed at least 1 gift
PHX
JSR gift_item_receipt   ; call our new routine to add in multiple items we just received
PLX
DEX
DEX
BRA gift_item_queue_loop

out_of_gifts:
; load our two-byte queue indicator
LDA $316C00
ASL A
TAX
out_of_gifts_loop:
TDC
STA $316C00,X
DEX
DEX
BPL out_of_gifts_loop
STZ $316C00      ; zero out the gift queue

no_gift_received:
SEP #$20
TDC
LDA #$01        ; we're done here, advance the event queue
JMP $9B5C



gift_item_receipt:
LDX $00
gift_item_loop:
LDA $1869,X
CMP $1A
BEQ gift_item_match
INX
CPX #$0100
BNE gift_item_loop
; if we have reached this point, we didn't match a pre-existing item. so now we will find the first empty slot and add it to the inventory
LDX $00
gift_item_empty_loop:
LDA $1869,X
CMP #$FF      ; no item here?
BEQ gift_item_empty_match
INX
BRA gift_item_empty_loop


gift_item_empty_match:
LDA $1A             ; load the item received
STA $1869,X
LDA $1B
STA $1969,X
RTS

gift_item_match:
LDA $1969,X
CLC
ADC $1B
CMP #$63      ; did we hit the 99 cap?
BCC gift_item_cap
LDA #$63
gift_item_cap:
STA $1969,X
RTS


org $C0840F
C0840F:
; we will just hijack the original render routine of outputting learning a spell, since it's unused
CMP #$18    ; is it a previously unused tag?
BNE C08050
JMP C0multi_player_render

C08050:
CMP #$1B    ; is it the learn spell tag?
BNE C0844B  ; branch if not
LDA $0205   ; load item to send to our multiworld recipient
BRA C083DA  ; and branch away to the original item fetch treasure code to parse it out

org $C0844B
C0844B:

org $C083DA
C083DA:

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
; $316000 - first two bytes are the item queue index
; the next 0x320 bytes are the items being sent from the Archipelago server
; repeat this 3 more times. the first copy will be the "live" copy, while the next three will be for each save slot respectively
; $316C00 - the first two bytes are the item gift queue index
; the next 0x3FE bytes are the items being gifted from other players in the multiworld. this will allow us to process up to 511 gifts at once
; once a gift has been processed, we will zero it out. this will have the side effect of gifts not being able to be re-received if a player starts a new game
; repeat this 3 more times. the first copy will be the "live" copy, while the next three will be for each save slot respectively
SEP #$20
LDA #$31
PHA
PLB       ; now we're switching to bank 31
LDX #$0000
REP #$20
blank_more_loop:
STZ $6000,X     ; we're going to be blanking $316000 through $3163FF. this will only blank the live queue. the live queue will get saved to its appropriate save slot down below
STZ $6C00,X     ; we're going to be blanking $316C00 through $316FFF. this will only blank the live queue. the live queue will get saved to its appropriate save slot down below
INX
INX
STZ $6000,X
STZ $6C00,X
INX
INX
CPX #$0400
BNE blank_more_loop
SEP #$20
PLB
RTS

;;;;;;;;;;;;;;;;;;;

; now we need to handle saving the queue, since players will inevitably not be playing at all times

SRAM_slot:
DW $0000
DW $0000     ; slot 1
DW $0400     ; slot 2
DW $0800     ; slot 3

SRAM_save:
; we will put off writing the SRAM markers until we are done saving the queue
PHA      ; save our slot indicator
ASL A
TAX
REP #$20
LDA SRAM_slot,X   ; load where in SRAM we will be saving the queue to
TAX
SEP #$20
LDY $00
SRAM_save_loop:
LDA $316000,X    ; load current queue byte
STA $316400,X    ; save it to this slot's queue
LDA $316C00,X    ; load gifts received
STA $317000,X    ; save it to this slot's queue
INY
INX
CPX #$0400
BNE SRAM_save_loop
PLA
STA $0224
RTS

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
LDA $317000,X     ; load gifts received
STA $316C00,X     ; save it to the live queue
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; this code is for the new menu that will be called from an NPC in the beginner's house to send gifts!
; receiving gifts will be handled automatically by talking to the NPC

C3send_menu_label:
DW $7915 : DB "Multiworld Gifting",$00   ; tilemap position - text and terminator

C3send_owned_screen2:
DW $7CB3 : DB "Owned:",$00
C3send_equipped_screen2:
DW $7BB3 : DB "Equipped:",$00

C3send_small_window:
DW $588B : DB $1C,$02

C3send_desc_window_screen2:
DW $618B : DB $1C,$03

C3send_item_window_screen2:
DW $62CB : DB $11,$08

C3send_owned_window_screen2:
DW $62F1 : DB $09,$08

C3send_small_window_screen2:
DW $608B : DB $1C,$02

C3send_gift:
JSR $3F99    ; font color selection
LDA #$7B     ; menu selection, initiate screen to send gifts
STA $26
JMP $01BA    ; go to main menu loop. it will jump below

C3send_gift_initiate:
; this menu will "borrow" much from the colloseum code in terms of appearance
LDA #$01
STA $2107    ; 64x32
JSR $1AE2    ; sets maximum scroll, initiate variables, etc. the max scroll is going to be based on the item count, which is 256 total items. may need to tweak this to account for larger multiworlds
JSR $7D25    ; set initial hotspots
JSR $6A28    ; clear BG2 tilemap A
JSR $6A2D    ; clear BG2 tilemap B
JSR $6A32    ; clear BG2 tilemap C
JSR $6A3C    ; clear BG3 tilemap A
JSR $6A41    ; clear BG3 tilemap B
JSR $6A46    ; clear BG3 tilemap C
JSR $6A4B    ; clear BG3 tilemap D
JSR $6A15    ; clear BG1 tilemap A
JSR $6A19    ; clear BG1 tilemap B
JSR $6A1E    ; clear BG1 tilemap C
LDY #C3send_small_window
JSR $0341   ; build the window
LDY #$AD92  ; the item description window
JSR $0341   ; build the window
LDY #$AD96  ; use the same window that the colloseum item list has
JSR $0341   ; build the window
LDY #C3send_desc_window_screen2
JSR $0341
LDY #C3send_small_window_screen2
JSR $0341
LDY #C3send_item_window_screen2
JSR $0341
LDY #C3send_owned_window_screen2
JSR $0341
JSR $0E52
JSR $A73D    ; build tilemap for descriptions. used originally by item menu and colloseum
LDY #C3send_menu_label   ; page header
LDA #$2C
STA $29     ; set text color to blue
JSR $02F9   ; draw the text
LDA #$20
STA $29
JSR C3send_world_type_desc      ; this call will clear out anything in the description buffer. normally it would output a description but for setup we just want the string cleared
JSR C3send_draw_player_names    ; get the list of players in this multiworld, and output them as text
JSR $0E28
JSR $0E36
JSR $0E6E
JSR $0E7C
JSR $1B0E
LDA #$01
TSB $45      ; clear out current description, present or not
STA $26      ; transition
JSR $1368
LDA #$7C    ; sustain the gift menu
STA $27
LDA #$02    ; cursor is on
STA $46
JSR $07B0    ; queue up cursor OAM
JMP $3541   ; BRT is 1 and also trigger NMI

C3send_draw_player_names:
JSR $83F7
LDY #$000A      ; displaying up to 10 names at once
C3send_text_continue:
PHY
JSR C3send_draw_player_names2    ; get a name and draw it
LDA $E6
INC A
INC A
AND #$1F
STA $E6
PLY
DEY
BNE C3send_text_continue
RTS

C3send_draw_player_names2:
LDX #$9E8B
STX $2181
TDC
LDA $E6
INC A
LDX #$0003
JSR $809F
REP #$20
TXA
STA $7E9E89
SEP #$20
LDA $E5
CMP $F10000
BCS C3send_text2_no_string
LDA #$F1
STA $F2
TDC
LDA $E5   ; load current position
REP #$20
ASL A     ; * 2
ASL A     ; * 4
ASL A     ; * 8
ASL A     ; * 16
INC A
INC A     ; and then add 2
STA $F0
TDC
SEP #$20
LDY $00
C3send_text2_continue:
LDA [$F0],Y
CMP #$FF
BEQ C3send_text2_end
STA $2180
INY
CPY #$0010
BEQ C3send_text2_end
BRA C3send_text2_continue
C3send_text2_end:
STZ $2180
INC $E5
JMP $7FD9

C3send_text2_no_string:
LDY #$0020
LDA #$FF
C3send_text2_no_string_loop:
STA $2180
DEY
BPL C3send_text2_no_string_loop:
STZ $2180
INC $E5
JMP $7FD9


C3send_gift_sustain:
LDA #$10
TRB $45    ; set menu flag, enable descriptions
LDA #$06    ; for L/R scrolling
STA $2A
JSR $0EFD
JSR $1F64   ; handle L/R for scrolling
BCS C3send_RTS2   ; exit if pressed
JSR $7D22     ; hotspot data
JSR C3send_world_type_desc   ; we will output what game the player is playing in the description box
LDA $08
BPL C3send_check_B_press    ; branch if we are not pressing A/confirm, and go check for B
; we are here if we pressed A on a player
; now we need 1 sanity check to make sure the player didn't select a blank spot
TDC
LDA $4B    ; load our position in the list. this determines who we are sending an item to.
; we aren't going to try and determine *if* the player can receive items, since the framework will do that for us. we only care to send the items out
CMP $F10000   ; compare it to the total number of players in this multiworld. right now there is one flaw with this, and that's if there are more than 255 people in the session
BCS C3send_invalid
; so if we press A/confirm, we have selected our world to send an item to
; which means we need to generate the current item list
STA $0201   ; save our recipient
LDA #$78
STA $26
JMP $0EB2    ; click

C3send_invalid:
JSR $0EC0   ; buzzer
JSR $305D   ; mosaic

C3send_item_list_initiate:
; we only need to clear the tilemap of the previous list, and then fill it with the items
; essentially all other menu work that previously exists for the other menu will work here
; C3/7E0D will populate our item list
; C3/82F1 will populate the item's description, if it has one
; JSR $6A3C    ; clear tilemap of previous list
JSR $7D25    ; set initial hotspots
JSR $7E0D    ; and now fill in the item list
LDA #$10
TSB $45      ; menu flags. turn off descriptions for now
JSR $82F1    ; item descriptions, but in this case clear out the buffer
JSR $1368    ; trigger NMI
LDA #$02
STA $46
JSR $1B0E     ; re-initialize the finger, the arrows, and do HDMA setup
LDA #$60
STA $26       ; since $1B0E puts #$01 into $26, we wait until after execution to set the next menu here
C3send_RTS2:
RTS

C3send_item_list_sustain:
LDA #$10
TRB $45      ; menu flags. turn the descriptions back on
STZ $2A     ; for L/R scrolling
JSR $0EFD
JSR $1F64   ; handle L/R for scrolling
BCS C3send_RTS
JSR $7D22
JSR $82F1    ; draw the item description of the current item if it has one
LDA $08
BPL C3send_check_B_press
; we are here if we are selecting an item. make sure we aren't trying to send nothing
TDC
LDA $4B     ; load position
TAX
LDA $1869,X  ; load item in slot
CMP #$FF     ; blank item?
BEQ C3send_invalid
STA $0205    ; store as our item to send
LDA $1969,X
STA $64      ; this will be the max we can possibly send out
JSR $0EB2    ; click
LDA #$4E     ; now we are going to call our "how many?" menu for a quantity to send
STA $26
RTS

C3send_check_B_press:
LDA $09
BPL C3send_RTS   ; branch and exit if not pressing B to exit out
JSR $0EC7   ; shift sound
STZ $26    ; fade out
LDA #$FF
STA $27    ; exit the menu
STA $0205   ; repurpose this to indicate to our NPC which item we are sending. in this case, clear it out since we are sending nothing
STA $0202   ; repurpose this to indicate to our NPC what quantity we are sending. in this case, clear it out since we are sending nothing
C3send_RTS:
RTS

C3send_how_many_initiate:
; now we need to make a makeshift screen for the player to select how many of the item they just selected to send out
; we will be borrowing elements from the "how many?" screen from the buy/sell menus in shops
LDA #$10
TRB $45      ; stop item descriptions from refreshing, but keep it displayed
LDA #$20
TSB $45      ; ignore joypad detection for descriptions. this effectively keeps the same one displayed at all times
LDA #$02
STA $46      ; turn off the scroll arrows
JSR $B94E    ; clear BG1; this removes the item list
JSR $0EFD
JSR $1368    ; trigger NMI; update screen and clear out item list
LDY $00
STY $3B      ; set BG2 Y-position
LDY #$0100
STY $39      ; set BG2 X-position
LDA #$01
STA $28      ; we will start with 1
JSR $04E0
LDX #$7BAB   ; position of number to gift
JSR $04B6
JSR $0F39
REP #$20
LDA #$7B8F
STA $7E9E89
SEP #$20
JSR $BFCB    ; get cursor position and point at inventory
JSR $C068    ; draw our item name
JSR $7FD9    ; output it to the tilemap
LDA #$2C
STA $29      ; set text color to blue
LDY #C3send_owned_screen2
JSR $02F9
LDY #C3send_equipped_screen2
JSR $02F9
LDA #$20
STA $29
JSR $BF66    ; display total number of this item that are equipped between all of your party members
TDC
LDA $4B
TAX
LDA $1969,X
JSR $04E0
LDX #$7D3F
JSR $04B6    ; display total number of this item that you own
LDX #$0008
STX $55
LDX #$0054
STX $57     ; set new finger X,Y coordinates
INC $26     ; sustain this how many menu
RTS


C3send_how_many_sustain:
; lots of code to do here
; we will only handle +/- 1 at a time
; first we check to see if we are pressing right
LDA $0B
LSR A      ; pressing right?
BCC C3send_quantity_check_left
LDA $28
CMP $64    ; can we give more?
BEQ C3send_quantity_check_A   ; branch and essentially do nothing if so. we can't give more than we have
INC $28
JSR $0EA3
BRA C3send_quantity_check_A

C3send_quantity_check_left:
; A already holds the contents of $0B and it's been shifted once
LSR A      ; pressing left?
BCC C3send_quantity_check_A
LDA $28
CMP #$01     ; are we down to 1?
BEQ C3send_quantity_check_A
DEC $28
JSR $0EA3

C3send_quantity_check_A:
LDA $28
JSR $04E0
LDX #$7BAB
JSR $04B6       ; update the quantity we are displaying

LDA $08
BPL C3send_check_B_press2    ; branch if we are not pressing A/confirm, and go check for B


; at long last, after everything has been confirmed in the menu, we exit out
LDA $28
STA $0202     ; we are sending this amount to the player
STZ $0203
LDA #$FF
STA $27
STZ $26
RTS

C3send_check_B_press2:
LDA $09
BPL C3send_RTS3   ; branch and exit if not pressing B to exit out
JSR $0EC7   ; shift sound
STZ $26    ; fade out
LDA #$FF
STA $27    ; exit the menu
STA $0205   ; repurpose this to indicate to our NPC which item we are sending. in this case, clear it out since we are sending nothing
STA $0202   ; repurpose this to indicate to our NPC what quantity we are sending. in this case, clear it out since we are sending nothing
C3send_RTS3:
RTS


C3gift_scroll:
JSR $7E95
JMP C3send_draw_player_names


C3send_world_type_desc:
; we must now output the player's game in the description box
LDX $00   ; #$0000
STX $E7   ; pointer address will be $F3/0000
STX $EB   ; make relativeness 0, effectively making the pointers absolute
LDA #$F3   ; set bank
STA $E9   ; full 24-bit address is now set
STA $ED   ; and bank is now set for relativeness
LDX #$9EC9
STX $2181
TDC
LDA $4B   ; load position
CMP $F10000   ; compare it to total number of players in this multiworld
BCS C3send_item_no_desc
JMP $5738   ; output our description, which in this case is our game name for our player 

C3send_item_no_desc:
LDA #$FF
JMP $576D    ; send a blank description indicator, effectively blanking it

; both of these LUTs are for the $2A value for scrolling our menus. we had to add two for the player select screen
C319AF:	DW $1FD0
C319B1:	DW $1FD6
C319B3:	DW $1FDC
C319B5:	DW $1FE2
C319B7:	DW $1FE8
C319B9:	DW $1FEE
C319BB:	DW C3gift_scroll

C382E5:	DW $7FA1
	DW $4F9E
	DW $5256
	DW $53EE
	DW $54E3
	DW $9CE2
	DW C3send_draw_player_names2

org $C329EE
JSR SRAM_load2     ; this will load up the slot of whatever you confirm on the load screen

org $C31523
JSR SRAM_save   ; save our SRAM, including our new queue

org $C31FBF
; we need to move the L/R scrolling LUT since we are adding an entry
JSR (C319AF,X)

org $C382E2
; we need to move an additional scrolling LUT since we are adding an entry
JSR (C382E5,X)


;;;;;;;;;;;;;;;;;;;

; now we need to make one change to an event script
; we will be hooking the NPC behind the counter at the Narshe School
; we will keep his original WoB/WoR check in for the dialogue, but then will need to add in dialogue and commands for the gifting of items
; then call one new final dialogue to confirm sending the item to the recipient

org $CC339C
; first, move our event to another location
DB $B2 : DL $01EFCA   ; call routine CB/EFCA
DB $FE   ; end script

org $CBEFCA
DB $D3,$79   ; clear bit 179, $1EAF:1
DB $C0 : DW $80A4 : DL OldCC33A6-$CA0000   ; if event bit $0A4 is set, (in WoR), branch to the address instead
DB $4B : DW $0257   ; this is a classroom for the beginner. think of us as your advisors
DB $91
DB $67   ; process any gifts sent to you
DB $C0 : DW $0179 : DL OldCCskipgift-$CA0000
DB $4B : DW $0004    ; the game has processed gifts sent to you. this dialogue message acknowledges it
OldCCskipgift:
DB $91
DB $4B : DW $0001
DB $A4     ; call menu
DB $66 : DW $0202 : DB $00    ; prepare our quantity for dialogue display. 8-bit range
DB $92
DB $C0 : DW $8179 : DL OldCCExitItem-$CA0000
DB $4B : DW $0002
DB $B6 : DL OldCCSendItem-$CA0000,$005EB3   ; branch based on selection in dialogue. in this case if you say yes it effectively branches forward 1 byte
OldCCExitItem:
DB $FE  ; end script

OldCCSendItem:
DB $E6  ; send out the item to the player in the multiworld session. this will also handle removing it from inventory
DB $93  ; pause for 45
DB $D3,$79   ; clear bit 179, $1EAF:1
DB $4B : DW $0003  ; placeholder
DB $FE

OldCC33A6:
DB $4B : DW $0258   ; we'll be here even if the world should crumble
DB $C0 : DW $0179 : DL OldCCskipgift-$CA0000
DB $4B : DW $0004    ; the game has processed gifts sent to you. this dialogue message acknowledges it
DB $91    ; pause for 15
DB $4B : DW $0001
DB $A4     ; call menu
DB $66 : DW $0202 : DB $00    ; prepare our quantity for dialogue display. 8-bit range
DB $92
DB $C0 : DW $8179 : DL OldCCExitItem-$CA0000
DB $4B : DW $0002
DB $B6 : DL OldCCSendItem-$CA0000,$005EB3   ; branch based on selection in dialogue. in this case if you say yes it effectively branches forward 1 byte
DB $FE  ; end script


org $CD0000
; replace the dialogue at the beginning of CD/0000 with our multiworld dialogue
incbin FF6_dialogue.bin

org $CCE602
; this is the pointer location for dialogue
; we need to change the first three pointers
DW $0000  ; the first entry is a dummy. keep it #$0000
DW $0000  ; the first string still starts at CD/0000
DW $002C  ; the second string is now at CD/002C
DW $0057  ; and the third string is now at CD/0055
DW $00A5


org $C080D2
; now we will hook part of our original dialogue decoding routine to add in outputting of a player name from the multiworld session
C080D2:
C080C7:	PLX 
C080C8:	STX $C9
C080CA:	PLA 
C080CB:	STA $CB
C080CD:	PLA 
C080CE:	STA $CF
C080D0:	RTS

CMP #$1A      ; are we trying to get an item name from a box?
BEQ C08118    ; branch if so
JSR C0send_check_characters
BCS C080D2    ; branch and exit if the parameter is between 2 and 16. this is outside our original character range
NOP
NOP
NOP           ; three bytes need to be NOP'ed out to maintain the original size

org $C08118
C08118:
