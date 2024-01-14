INCLUDE "hardware.inc"

; In this game you control a pad, there is a ball bouncing off walls (left, right and top) and off the pad, your objective is to move the pad to avoid the ball going below the pad.
;
;
; CODE RULES:
;	The only method to turn off LCD is TurnLCDOff, you must wait VBlank before calling this method.
;	Running TurnLCDOff causes disabling interrupts, waiting VBlank, turning LCD off and the program will loop eternally throwing ERROR_ATTEMPT_LCD_OFF_OUTSIDE_VBLANK
;	Use jr and ldh when it is possible
;
;	If drawing o logic operations exceeds its corresponding PPU Mode: LCD will turn off, interrupts disabled and the program will loop eternally throwing an error
;
;
; Ball Movement
; Python code to generate precalculated table
; 	Y:
;		for i in range(256): print('\tdb '+str(round(math.sin(math.pi * (i * (2 / 256))) * 127)))
;	X:
;		for i in range(256): print('\tdb '+str(round(math.cos(math.pi * (i * (2 / 256))) * 127)))
;
; Angle is changed when ball touches left/right wall, top wall or pad
; TODO (angle change behavior)

; GENERAL DEFINITIONS
DEF OAMRAM_SIZE EQU 160
DEF WRAM_DMA_SIZE EQU 160

; GAME DEFINITIONS
DEF GAMESTATE_RUNNING EQU 16

DEF PLAYER_ID EQU 0
DEF BALL_ID EQU 1

DEF PLAYER_START_Y EQU 128 + 16
DEF PLAYER_START_X EQU 16 + 8

DEF BALL_START_Y EQU 16 ; Ball initial Y & X position on screen (0-255)
DEF BALL_START_X EQU 8
DEF BALL_START_DIR EQU 32 ; Ball initial direction (45º)

; PROGRAM STATUS
; wProgramStatus
DEF READY EQU 0
DEF GRAPHIC_OPERATIONS_RUNNING EQU 1
DEF LOGIC_OPERATIONS_RUNNING EQU 2

; ERROR CODES
; wErrorRegister
DEF ERROR_LOGIC_INSIDE_VBLANK EQU 1
DEF ERROR_GRAPHICS_OUTSIDE_VBLANK EQU 2
DEF ERROR_GRAPHICS_EXCEED_HBLANK EQU 3
DEF ERROR_ATTEMPT_LCD_OFF_OUTSIDE_VBLANK EQU 4

; Memory locations
DEF WRAM_OBJECTS EQU $C000 ; Location in WRAM where object attributes are stored


; -------------------------

SECTION "VBlank Interrupt", ROM0[$40] ; Called when VBlank begins
jp VBlankInterrupt

SECTION "Header", ROM0[$100]

	jr EntryPoint

	ds $150 - @, 0 ; Make room for the header

EntryPoint: ; Setup
	ld a, 0 ; Shut down audio circuitry
	ldh [rNR52], a
	call InitRAM ;
	call LoadGame ; Turn off LCD, load textures to VRAM, init OAM & set LCD registers (Turn on LCD)
	ld a, IEF_VBLANK ; Set VBlank interrupt flag
	ldh [rIE], a
	xor a, a ; Same as ld a, 0
	ldh [rIF], a ; Set VBlank (and rest of the bits) of Flag Register to 0. To prevent calling the interrupt just after enabling interrupts
	ei ; Enable interrupts
	jp Main ; Jump to main loop
	
	
	
; -------------------------
; MEMORY

InitRAM: ; Clean HRAM, clean WRAM used by DMA transfer ($C000 - $C09F), copy DMA transfer code, copy SignedAdd code to HRAM
	ld a, 0 ; Load 0 to A register. This value will be used to write it on HRAM addresses used by the game
	ld hl, StartHRAM
	ld b, EndHRAM - StartHRAM
	call CleanMem
	
	ld hl, _RAM ; Clean WRAM used by DMA transfer
	ld b, WRAM_DMA_SIZE
	call CleanMem
	
	ld de, StartDMACode ; Copies DMA code from ROM to HRAM
	ld hl, runDMA
	ld bc, EndDMACode - StartDMACode
	call CopyMem
	
	ld de, StartSignedAddCode ; Copies SignedAdd code from ROM to HRAM
	ld hl, runSignedAdd
	ld bc, EndSignedAddCode - StartSignedAddCode
	call CopyMem
	ret
	
; @param a: value to write
; @param hl: start location
; @param b: size
CleanMem: ; Clean memory with zeros
	ld [hli], a 
	dec b
	jr nz, CleanMem
	ret
	
; @param de: from
; @param hl: to
; @param: bc: size (bytes)
CopyMem: ; Copy bytes from ROM to another location
	ld a, [de]
	ld [hli], a
	inc de
	dec bc
	ld a, b
	or a, c
	jr nz, CopyMem
	ret
	
DMACopy: ; Copies 160 bytes from WRAM to OAM (Source: $C000 - $C09F, Destination: $FE00 - $FE9F)
	; Writing to register $FF46 (DMA) Starts OAM DMA transfer
	; Value specifies Source address divided by $100, in this case value is $C0
	; Transfer takes 160 cycles: 640 dots (1.4 lines)
	; Code executed to wait those cycles must be in HRAM. During DMA copy CPU cannot access ROM
	ld a, HIGH(WRAM_OBJECTS)
	ld bc, $2846 ; $28 = wait time (40 * 4 = 160 cycles), $46 = LOW(FF46)
	jp runDMA ; Jump to code in HRAM
	; Return is made from HRAM

; This functions adds one signed byte to a word, it uses SP so at the end of the fucntion SP will return to its initial value
; This function jumps and returns from code loaded into HRAM.
; @param hl: word to which the signed will be added
; @param a: signed byte to add to hl
; @return hl: result
SignedAdd:
	ldh [wSignedToAdd], a ; Store value on register A into wSignedToAdd address
	ld [wStackPointer], sp ; Store SP value at address wStackPointer
	ld sp, hl ; Load HL into SP register
	jp runSignedAdd ; Jump to code in HRAM
	; Return is made from HRAM

; -------------------------
; LCD

WaitVBlank:
	ldh a, [rLY] ; Wait VBlank
	cp 144
	jr c, WaitVBlank
	ret
	
TurnLCDOff: ; Turns LCD off
	ldh a, [rLY] ; Checks if runs outside VBlank
	cp 144
	jp c, ErrorLCDOffOutsideVBlank ; Stop & throw error if it is running outside VBlank
	ld a, 0 ; Turn off screen
	ldh [rLCDC], a
	ret

TurnLCDOn: ; Turns LCD on and sets LCD registers
	ld a, LCDCF_ON | LCDCF_BGON | LCDCF_OBJON ; Set flags: LCD, Background and Objects
	ldh [rLCDC], a
	; During the first (blank) frame, initialize display registers
	ld a, %11100100
	ldh [rBGP], a
	
	ld a, %11100100
    ldh [rOBP0], a
	ret
	
	
	
; -------------------------
; GRAPHICS

LoadGame: ; Only called during initialization. Turns off LCD, loads textures, cleans and initializes OAM and then turns LCD on setting registers
	call WaitVBlank
	call TurnLCDOff
	call LoadTextures
	
	call InitOAM
	
	ld a, GAMESTATE_RUNNING ; Set game state to running
	ld [wGameState], a
	
	call TurnLCDOn
	ret

LoadTextures: ; Load textures from ROM to VRAM
	; LCDC.4 = 0: Background on $9000 (0 - 127), Objects on $8000 (0 - 127)
	ld de, ObjectTiles
	ld hl, _VRAM ; $8000
	ld bc, EndObjectTiles - ObjectTiles
	call CopyMem
	;
	ld de, BackgroundTiles
	ld hl, _VRAM9000 ; $9000
	ld bc, EndBackgroundTiles - BackgroundTiles
	call CopyMem
	; LCDC.3 = 0: Background tilemap on $9800
	ld de, BackgroundTilemap
	ld hl, _SCRN0 ; $9800
	ld bc, EndBackgroundTilemap - BackgroundTilemap
	call CopyMem
	ret
	
InitOAM: ; Initialize objects (player & ball)

	ld a, 0 ; Clean OAM & Set object attributes in WRAM
	ld hl, _OAMRAM
	ld b, OAMRAM_SIZE
	call CleanMem ; Clean ORAM
	
	; Object attributes are stored in RAM, next in the game loop will be copied to OAM using DMA Copy
	ld hl, WRAM_OBJECTS
	ld a, PLAYER_START_Y ; Player
	ld [hli], a 
	ld a, PLAYER_START_X
	ld [hli], a
	ld a, PLAYER_ID ; Player id & attributes are 0 so this can be changed by increasing hl twice
	ld [hli], a 
	ld [hli], a
	
	ld a, BALL_START_Y ; Ball
	ld [hli], a 
	ld a, BALL_START_X
	ld [hli], a 
	ld a, BALL_ID
	ld [hli], a 
	xor a, a ; Same as ld a, 0
	ld [hl], a	
	
	ld a, BALL_START_Y ; Store ball position into the first byte of the word used to calculate ball Y & X position
	ld [wCalcBallY], a
	ld a, BALL_START_X
	ld [wCalcBallX], a
	ld a, BALL_START_DIR ; Store ball initial direction into wDIr address
	ld [wDir], a
	
	ret

UpdateGraphics: ; Uses DMA transfer to update OAM. Make sure it doesn't overlap OAM Scan Period (Mode 2), also update VRAM (tiles) CHANGE TODO
	call DMACopy
	ret

	
; -------------------------
; KEYS

CheckKeys:
	; $FF00 stores keys pressed (rP1)
	; Write $DF (bit 5 low) on $FF00 to get START, SELECT, A, B group of keys
	ld a, $DF ;1101 1111
	ld [rP1], a
	call .knownRet ; Wait (TODO: Check if is the best way)
	ld a, [rP1] ; Read keys
	xor $DF ; Get Only first nibble low bits.
	ld b, a ; Store these keys on b
	
	ld a, $EF ;1110 1111
	ld [rP1], a
	call .knownRet
	ld a, [rP1] ; Read keys
	xor $EF ; Get Only first nibble low bits
	swap a ; Move bits to left nibble
	or b ; Add bits from first read to right nibble
	; Now `a` register contains keys, bits left to right: down, up, left, right, start, select, b, a
	; Save `a` (buttons) on `b`
	ld b, a
	call CheckStartBtn
	; Retrieve buttons from `b`
	ld a, b
	call CheckDpad
	
.knownRet: ; Calling this function wastes 10 cycles (Source: https://rgbds.gbdev.io/docs/v0.6.1/gbz80.7) (TODO: check)
	ret

CheckStartBtn:	
	; Pause / Resume
	and $08 ; Get only bit 3, set flags (check pressed / released)
	ld a, [wGameState] ; Load game state
	
	jp z, .unpressed ; If bit 3 is low (START released)
	; If pressed
	; Rotate left if 16 or 64
	and 80
	call nz, .switchGameState
	ret ; If not, return

.unpressed:
	; Rotate left if 32 or 128, if carry swap nibbles
	and 160
	call nz, .switchGameState
	ret ; if not, return

.switchGameState:
	ld a, [wGameState]
	rlca ; Rotate left
	jp nc, .noCarry
	swap a ; If carry, swap nibbles (keep high bits on the left nibble)
.noCarry:
	ld [wGameState], a ; Set new game state
	ret
	
CheckDpad:
	; Left / Right
	and $30 ; Get only bit 4 & 5: set flag z
	ret z ; If no one of the two keys are pressed, return
	and $10 ; Get only right key, set flag
	ld a, [wPlayerX] ; Load player X position to a
	jp nz, .right; No right key press, left key is pressed
	; Move player X position left
	; The object most left pixels dissapear at X position 7
	cp 8 ; If the next X position is going to be < 8, don't move
	ret c
	dec a ; To move left, decrease X position
	jp .save
.right: ; Move player X position right
	; Screen width is 160 pixels, X position 160 + 8 ( because first 8 positions are out of the screen) match with the first pixel out of the screen in the right side
	; 160 corresponds with 160 + 8 (left margin) - 8 (object width) because we don't want to hide the object behind the right margin
	cp 160 ; If the next position is going to be >= 160, don't move
	ret nc
	inc a	 ; To move right increase X position
.save:
	ld [wPlayerX], a
	ret


	
; -------------------------
; ERROR HANDLING

TurnLCDOffOnError: ; Disables interrupts, waits VBlank, turns LCD off and returns
	di
	call WaitVBlank
	call TurnLCDOff
	ret

; Error functions: Call TurnLCDOffOnError, store error on program registers in HRAM and loop eternally
ErrorLogicInsideVBlank: ; Main loop jumps here when logic operations exceed not VBlank Period
	call TurnLCDOffOnError
	ld a, ERROR_LOGIC_INSIDE_VBLANK
	ldh [wErrorRegister], a
	jr Main

ErrorGraphicsOutsideVBlank: ; Main loop jumps here when graphic operations exceed VBlank Period
	call TurnLCDOffOnError
	ld a, ERROR_GRAPHICS_OUTSIDE_VBLANK
	ldh [wErrorRegister], a
	jr Main
	
ErrorGraphicsExceedHBlank: ; VBlankInterrupt jumps here when is called but last VBlankInterrupt did not finish graphic operations
	call TurnLCDOffOnError
	ld a, ERROR_GRAPHICS_EXCEED_HBLANK
	ldh [wErrorRegister], a
	jr Main
	
ErrorLCDOffOutsideVBlank: ; TurnLCDOff jumps here if it has been called outside VBlank
	call TurnLCDOffOnError
	ld a, ERROR_ATTEMPT_LCD_OFF_OUTSIDE_VBLANK
	ldh [wErrorRegister], a
	jr Main
	
	
	
; -------------------------

UpdateGame: ; Updates game state in RAM
	call CheckKeys ; Check keys pressed & move pad
	call UpdateBallPosition ; Move ball according to its direction
	ret
	
UpdateBallPosition: ; Move ball (wCalcBallY & wCalcBallX) according to its direction (wDir).
	; This code searches on 2 tables, each one contains 256 values for delta position, ball direction is used as index for this table
	; Result is added to wCalcBallY / wCalcBallX (2 bytes), only left byte is used for real (on-screen) ball position
	ld a, [wDir] ; Load byte at wDir address into A
	ld b, 0 ; Set B register to c
	ld c, a ; Load A value into C, BC will have the value in the address wDir
	
	ld hl, BallYCoords ; Load BallYCoords address into HL register
	add hl, bc ; Append ball direction index to BallYCoords address to get Y value
	ldh a, [wCalcBallY] ; Load word in wCalcBallY into DE register
	ld d, a
	ldh a, [wCalcBallY + 1]
	ld e, a
	push de ; Save DE on the stack
	ld a, [hl] ; Load value (Y coord) at address in HL to A register
	pop hl ; Retrieve previously saved DE value on the stack to the HL register
	call SignedAdd ; add signed byte on A to HL
	ld a, h ; Store result word first byte into address wCalcBallY
	ldh [wCalcBallY], a
	ld [wBallY], a ; Store result word first byte into address wBallY (Real position)
	ld a, l ; Store result word second byte into address next to wCalcBallY
	ldh [wCalcBallY + 1], a
	
	ld hl, BallXCoords ; Load BallXCoords address into HL register
	add hl, bc ; Append ball direction index to BallXCoords address to get X value
	ldh a, [wCalcBallX] ; Load word in wCalcBallX into DE register
	ld d, a
	ldh a, [wCalcBallX + 1]
	ld e, a
	push de ; Save DE on the stack
	ld a, [hl] ; Load value (X coord) at address in HL to A register
	pop hl ; Retrieve previously saved DE value on the stack to the HL register
	call SignedAdd ; add signed byte on A to HL
	ld a, h ; Store result word first byte into address wCalcBallX
	ldh [wCalcBallX], a
	ld [wBallX], a ; Store result word first byte into address wBallX (Real position)
	ld a, l ; Store result word second byte into address next to wCalcBallX
	ldh [wCalcBallX + 1], a
	
	ret

VBlankInterrupt: ;
	ei ; Enable interrupts again, allow the next VBlank interrupt to check if operations running on this interrupt have been finished
	ldh a, [wProgramStatus] ; Check logic & graphic operations ended
	cp GRAPHIC_OPERATIONS_RUNNING
	jp z, ErrorGraphicsExceedHBlank
	jr nc, ErrorLogicInsideVBlank
	inc a ; Set graphic operations running
	ldh [wProgramStatus], a
	call UpdateGraphics ; Update graphics (VRAM & OAM)
	ldh a, [rLY]; Check if still in VBlank
	cp 144
	jr c, ErrorGraphicsOutsideVBlank
	ld a, LOGIC_OPERATIONS_RUNNING ; Set logic operations running
	ldh [wProgramStatus], a
	call UpdateGame ; Update game logic (don't touch VRAM) TODO
	ld a, READY ; Set graphic & logic operations ended
	ldh [wProgramStatus], a
	ret ; Return and enable interrupt
	
Main: ; Main loop
	jr Main
	
	
	
; -------------------------
; DMA Code: Must to be copied into HRAM

StartDMACode:
	ldh [c], a ; Load source address into DMA register
.wait ; Takes 4 cycles
	dec b ; 1 cycle
	jr nz, .wait ; 3 cycles
	ret z ; Conditional `ret` is 1 cycle slower, which avoids reading from the stack on the last cycle of DMA.
EndDMACode:


; SignedAdd Code: Must be copied into HRAM
; Some of this instructions will be modified on demand so pointers wSignedToAdd and wStackPointer points to an instruction argument
StartSignedAddCode:
	add sp, $FF ; Add signed byte to stack pointer. Instruction argument (byte) address matches with wSignedToAdd pointer
	ld hl, sp+0 ; Load SP value into HL
	ld sp, $FFFF ; Load word into stack pointer. Instruction argument (word) address matches with wStackPointer pointer
	ret
EndSignedAddCode:

; -------------------------

SECTION "WRAM Objects", WRAM0 ; $C000
	; Starting at address $C000
	; Each object has a size of 4 bytes
	; 1: X position, 2: Y position, 3: Tile ID, 4: Attributes

	db ; Start of Player attributes (Points Player Y)
	wPlayerX: ds 3 ; Player X position, reserves 3 bytes to fill player attributes
	
	wBallY: db ; Ball Y real position and the start of ball attributes
	wBallX: ds 3 ; Ball X real position, next 3 bytes are ball attributes
	
	
SECTION "HRAM", HRAM 
	StartHRAM: 
	; DMA transfer function
	runDMA: ds 5; Reserves 5 bytes
	
	; SignedAdd function, uses 8 bytes
	runSignedAdd: db ; Reserve 1 byte for ADD SP, r8 opcode
	wSignedToAdd: db ; Holds signed byte to add to SP
	ds 3 ; Reserve 3 bytes for LD HL, SP + r8 instruction and LD SP, d16 opcode
	wStackPointer: dw ; Holds SP value temporally
	db ; Last function instruction (return)
	
	
	; Program registers
	; Holds variables used to check and notify critial errors
	
	; @value 0: Graphic & logic operations ended
	; @value 1: Graphic operations are running
	; @value 2: Graphic operations ended, logic operations are running
	wProgramStatus: db ; Useful to check if logic / graphic operations are running inside the correct PPU modes
	
	; @value 1: Logic operations exceed not VBlank period
	; @value 2: Graphic operations exceed VBlank period
	; @value 3: Graphic operations exceed HBlank (running inside next VBlank)
	; @value 4: TurnLCDOff called outside VBlank period
	wErrorRegister: db ; Holds value describing error
	
	
	; Game Registers
	; Holds variables used during gameplay

	; Hold game state (resumed / paused) values are 16 -> 32 -> 64 -> 128, 16 means running, else means paused
	; Values are changed by rotating left, only left nibble is used, when carry a swap moves bit 0 to 5th position
	wGameState: db
	
	; This two values store the more precise position of the ball (2 bytes), this are used to calculate ball position adding delta position.
	; Only left byte is used for real (screen) position, this leaves the right byte for something like decimals
	wCalcBallY: dw
	wCalcBallX: dw
	
	; Ball direction starting from right, clockside
	wDir: db
	
	EndHRAM:
	
	
	
; -------------------------
; TILES
SECTION "Textures", ROM0

ObjectTiles:
	dw `00000000 ; $00: Player
	dw `00000000
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `00000000
	dw `00000000
	dw `00033000 ; $01: Ball
	dw `00333300
	dw `03333330
	dw `33333333
	dw `33333333
	dw `03333330
	dw `00333300
	dw `00033000
EndObjectTiles:
	
BackgroundTiles:
	dw `33333333 ; $00
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `22222222 ; $01
	dw `22222222
	dw `22222222
	dw `22222222
	dw `22222222
	dw `22222222
	dw `22222222
	dw `22222222
	dw `11111111 ; $02
	dw `11111111
	dw `11111111
	dw `11111111
	dw `11111111
	dw `11111111
	dw `11111111
	dw `11111111
	dw `00000000 ; $03
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
EndBackgroundTiles:

BackgroundTilemap:
	db $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03,  0,0,0,0,0,0,0,0,0,0,0,0
	db $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02,  0,0,0,0,0,0,0,0,0,0,0,0
	db $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03,  0,0,0,0,0,0,0,0,0,0,0,0
	db $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02,  0,0,0,0,0,0,0,0,0,0,0,0
	db $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03,  0,0,0,0,0,0,0,0,0,0,0,0
	db $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02,  0,0,0,0,0,0,0,0,0,0,0,0
	db $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03,  0,0,0,0,0,0,0,0,0,0,0,0
	db $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02,  0,0,0,0,0,0,0,0,0,0,0,0
	db $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03,  0,0,0,0,0,0,0,0,0,0,0,0
	db $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02,  0,0,0,0,0,0,0,0,0,0,0,0
	db $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03,  0,0,0,0,0,0,0,0,0,0,0,0
	db $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02,  0,0,0,0,0,0,0,0,0,0,0,0
	db $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03,  0,0,0,0,0,0,0,0,0,0,0,0
	db $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02,  0,0,0,0,0,0,0,0,0,0,0,0
	db $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03,  0,0,0,0,0,0,0,0,0,0,0,0
	db $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02,  0,0,0,0,0,0,0,0,0,0,0,0
	db $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03,  0,0,0,0,0,0,0,0,0,0,0,0
	db $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02, $03, $02,  0,0,0,0,0,0,0,0,0,0,0,0
EndBackgroundTilemap:


; -------------------------
; BALL DIRECCIONS
; These coords are previously calculated using sin() & cos() with the ball direction 0 - 255, resulting in values from -127 to 127
; These values represent the delta position of the ball according to its direction
; Coded in Two's Complement:
; https://www.allmath.com/twos-complement.php
; https://en.wikipedia.org/wiki/Signed_number_representations
BallYCoords:
	db 0
	db 3
	db 6
	db 9
	db 12
	db 16
	db 19
	db 22
	db 25
	db 28
	db 31
	db 34
	db 37
	db 40
	db 43
	db 46
	db 49
	db 51
	db 54
	db 57
	db 60
	db 63
	db 65
	db 68
	db 71
	db 73
	db 76
	db 78
	db 81
	db 83
	db 85
	db 88
	db 90
	db 92
	db 94
	db 96
	db 98
	db 100
	db 102
	db 104
	db 106
	db 107
	db 109
	db 111
	db 112
	db 113
	db 115
	db 116
	db 117
	db 118
	db 120
	db 121
	db 122
	db 122
	db 123
	db 124
	db 125
	db 125
	db 126
	db 126
	db 126
	db 127
	db 127
	db 127
BallXCoords:
	db 127
	db 127
	db 127
	db 127
	db 126
	db 126
	db 126
	db 125
	db 125
	db 124
	db 123
	db 122
	db 122
	db 121
	db 120
	db 118
	db 117
	db 116
	db 115
	db 113
	db 112
	db 111
	db 109
	db 107
	db 106
	db 104
	db 102
	db 100
	db 98
	db 96
	db 94
	db 92
	db 90
	db 88
	db 85
	db 83
	db 81
	db 78
	db 76
	db 73
	db 71
	db 68
	db 65
	db 63
	db 60
	db 57
	db 54
	db 51
	db 49
	db 46
	db 43
	db 40
	db 37
	db 34
	db 31
	db 28
	db 25
	db 22
	db 19
	db 16
	db 12
	db 9
	db 6
	db 3
	db 0
	db -3
	db -6
	db -9
	db -12
	db -16
	db -19
	db -22
	db -25
	db -28
	db -31
	db -34
	db -37
	db -40
	db -43
	db -46
	db -49
	db -51
	db -54
	db -57
	db -60
	db -63
	db -65
	db -68
	db -71
	db -73
	db -76
	db -78
	db -81
	db -83
	db -85
	db -88
	db -90
	db -92
	db -94
	db -96
	db -98
	db -100
	db -102
	db -104
	db -106
	db -107
	db -109
	db -111
	db -112
	db -113
	db -115
	db -116
	db -117
	db -118
	db -120
	db -121
	db -122
	db -122
	db -123
	db -124
	db -125
	db -125
	db -126
	db -126
	db -126
	db -127
	db -127
	db -127
	db -127
	db -127
	db -127
	db -127
	db -126
	db -126
	db -126
	db -125
	db -125
	db -124
	db -123
	db -122
	db -122
	db -121
	db -120
	db -118
	db -117
	db -116
	db -115
	db -113
	db -112
	db -111
	db -109
	db -107
	db -106
	db -104
	db -102
	db -100
	db -98
	db -96
	db -94
	db -92
	db -90
	db -88
	db -85
	db -83
	db -81
	db -78
	db -76
	db -73
	db -71
	db -68
	db -65
	db -63
	db -60
	db -57
	db -54
	db -51
	db -49
	db -46
	db -43
	db -40
	db -37
	db -34
	db -31
	db -28
	db -25
	db -22
	db -19
	db -16
	db -12
	db -9
	db -6
	db -3 ; End BallYCoords
	db 0
	db 3
	db 6
	db 9
	db 12
	db 16
	db 19
	db 22
	db 25
	db 28
	db 31
	db 34
	db 37
	db 40
	db 43
	db 46
	db 49
	db 51
	db 54
	db 57
	db 60
	db 63
	db 65
	db 68
	db 71
	db 73
	db 76
	db 78
	db 81
	db 83
	db 85
	db 88
	db 90
	db 92
	db 94
	db 96
	db 98
	db 100
	db 102
	db 104
	db 106
	db 107
	db 109
	db 111
	db 112
	db 113
	db 115
	db 116
	db 117
	db 118
	db 120
	db 121
	db 122
	db 122
	db 123
	db 124
	db 125
	db 125
	db 126
	db 126
	db 126
	db 127
	db 127
	db 127 ;End BallXCoords