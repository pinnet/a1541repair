                .CR     6502
                .TF     devkit.int,INT,8
                .LF     devkit.list



;-----------------------------------------------------------------------------------------------------------------------------
;
;  The WOZ Monitor for the Apple 1
;  Written by Steve Wozniak 1976
;  Adapted for VIA DEVKIT BY Danny Arnold (c) 2021
;-------------------------------------------------------------------------


;-------------------------------------------------------------------------
;  Memory declaration
;-------------------------------------------------------------------------

XAML            .EQ     $24             Last "opened" location Low
XAMH            .EQ     $25             Last "opened" location High
STL             .EQ     $26             Store address Low
STH             .EQ     $27             Store address High
L               .EQ     $28             Hex value parsing Low
H               .EQ     $29             Hex value parsing High
YSAV            .EQ     $2A             Used to see if hex value is given
MODE            .EQ     $2B             $00=XAM, $7F=STOR, $AE=BLOCK XAM
TMP0            .EQ     $C1
STRL            .EQ     $2C
STRH            .EQ     $2D 
MEMSZL          .EQ     $2E
MEMSZH          .EQ     $2F
BCD             .EQ     $10
NUMSTR          .EQ     $16
ZPLCOUNT        .EQ     $23

IN              .EQ     $0200,$027F     Input buffer

KBD             .EQ     $D010           PIA.A keyboard input
KBDCR           .EQ     $D011           PIA.A keyboard control register
DSP             .EQ     $D012           PIA.B display output register
DSPCR           .EQ     $D013           PIA.B display control register

VIAORA          .EQ     $8001           VIA Output Register A
VIAIRA          .EQ     $8001           VIA Input Register A
VIADDRA         .EQ     $8003           VIA Data Direction Register A

VIAORB          .EQ     $8000           VIA Output Register B
VIAIRB          .EQ     $8000           VIA Input Register B    
VIADDRB         .EQ     $8002           VIA Data Direction Register B  

VIASR           .EQ     $800A           VIA Shift Register
VIAACR          .EQ     $800B           VIA Aux Control Register
VIAPCR          .EQ     $800C           VIA Peripheral Control Register
VIAIFR          .EQ     $800D           VIA Interrupt Flag Register
VIAIER          .EQ     $800E           VIA Interrupt Enable Register

RST				.EQ     %01000000	
MOSI            .EQ     %10000000
MISO            .EQ     %00000010
CLK             .EQ     %00000001
CS              .EQ     %00000100



; KBD b7..b0 are inputs, b6..b0 is ASCII input, b7 is constant high
;     Programmed to respond to low to high KBD strobe
; DSP b6..b0 are outputs, b7 is input
;     CB2 goes low when data is written, returns high when CB1 goes high
; Interrupts are enabled, though not used. KBD can be jumpered to IRQ,
; whereas DSP can be jumpered to NMI.

;-------------------------------------------------------------------------
;  Constants
;-------------------------------------------------------------------------

BS              .EQ     $DF             Backspace key, arrow left key
CR              .EQ     $8D             Carriage Return
ESC             .EQ     $9B             ESC key
PROMPT          .EQ     '>'             Prompt character

;----------------------------------------------------------------------------------------------------------
;  Begin DevKit ROM
;----------------------------------------------------------------------------------------------------------
                .OR     $E000
                .TA     $0000

START         ; Setup VIA and reset SPI SLAVE
                LDA     #$D7                ; 0.001164 * 215
                STA     ZPLCOUNT            ; approx .25/sec loop

                LDA     #$FF 
                STA     VIADDRA             ; portA all OUTPUT
                LDA     #RST
                STA     VIAORA              ; set RST pin HIGH
                JSR     DELY                ; wait .25/sec
                LDA     #$00                
                STA     VIAORA              ; set all portA outputs low
                JSR     DELY                ; wait .25/sec
                LDA     #RST
                STA     VIAORA              ; bring RTS pin HIGH
                LDA     #$FF
                EOR     #RST                
                EOR     #MISO               
                STA     VIADDRA             ; Set DDRA OIOOOOIO

                


;---------------------------------------   Output Header to PIA            
                LDA     #STARTMSG &$FF
                STA     STRL
                LDA     #STARTMSG>>8
                STA     STRH
                JSR     STRECHO 

;----------------------------------------------------------------------------------------------------------                
EVRLOOP         JMP     EVRLOOP
;----------------------------------------------------------------------------------------------------------                

DELY            PHP                    ; 3   Save State
                PHA                    ; 3
                TXA                    ; 2
                PHA                    ; 3
                TYA                    ; 2
                PHA                    ; 3    16 total

                LDY     ZPLCOUNT       ; 3
.LP             LDX     #$FF           ; 2
.LP1            DEX                    ; 2
                BNE     .LP1           ; 2        4 x 256 = 1124
                DEY                    ; 2
                BNE     .LP            ; 2    1124 total

                PLA                    ; 4    Retrive State
                TAY                    ; 2
                PLA                    ; 4
                TAX                    ; 2
                PLA                    ; 4
                PLP                    ; 4    20 total
                RTS                    ;
                                       ;     1 iteration = 1164 cycles = 0.001164/sec


;----------------------------------------------------------------------------------------------------------
;   Messages
STARTMSG        .AZ     #$01,/ -----------------------------     Menu     --------------------------------- /,#$0D,#$02,#$0D
;----------------------------------------------------------------------------------------------------------
;.proc spibyte
;	        sta spi_writebuffer
;.repeat 8			                    ; copy the next section 8 times
;.scope
;	        lda #%01111000		        ; base DATAB value with chip select for
				                        ; MAX3100 and a zero bit in the output
				                        ; line.
;	        rol spi_writebuffer
;	        bcc writing_zero_bit
;	        ora #%00000100		        ; write a 1 bit to the output line.
;           writing_zero_bit:
		
; 	        sta VIA_DATAB    	        ; write data back to the port
;	        inc VIA_DATAB    	        ; set clock high

;	        lda VIA_DATAB		        ; Read input bit
;	        rol			                ; Shift input bit to carry flag
;	        rol spi_readbuffer      	; Shift carry into readbuffer

;	        dec VIA_DATAB    	        ; set clock low
;.endscope
;.endrepeat

;	        lda spi_readbuffer	        ; result goes in A
;	        rts
;.endproc
;-------------------------------------------------------------------------
                .NO     $FD00
                .TA     $1D00
;-------------------------------------------------------------------------
;       Vector Table
;-------------------------------------------------------------------------
JMP_VEC
START_VEC       JMP     START
;-------------------------------------------------------------------------
;  Let's get started
;
;  Remark the RESET routine is only to be entered by asserting the RESET
;  line of the system. This ensures that the data direction registers
;  are selected.
;------------------------------------------------------------------------                
RESET           CLD                     Clear decimal arithmetic mode
                CLI
                LDY     #%0111.1111     Mask for DSP data direction reg
                STY     DSP              (DDR mode is assumed after reset)
                LDA     #%1010.0111     KBD and DSP control register mask
                STA     KBDCR           Enable interrupts, set CA1, CB1 for
                STA     DSPCR            positive edge sense/output mode.

; Program falls through to the GETLINE routine to save some program bytes
; Please note that Y still holds $7F, which will cause an automatic Escape
                
                LDA     #BOOTMSG&$FF
                STA     STRL
                LDA     #BOOTMSG>>8
                STA     STRH
                JSR     STRECHO
                LDA     #0                     
;-------------------------------------------------------------------------
; The GETLINE process
;-------------------------------------------------------------------------

NOTCR           CMP     #BS             Backspace key?
                BEQ     BACKSPACE       Yes
                CMP     #ESC            ESC?
                BEQ     ESCAPE          Yes
                INY                     Advance text index
                BPL     NEXTCHAR        Auto ESC if line longer than 127

ESCAPE          LDA     #PROMPT         Print prompt character
                JSR     ECHO            Output it.

GETLINE         LDA     #CR             Send CR
                JSR     ECHO

                LDY     #0+1            Start a new input line
BACKSPACE       DEY                     Backup text index
                BMI     GETLINE         Oops, line's empty, reinitialize

NEXTCHAR        LDA     KBDCR           Wait for key press
                BPL     NEXTCHAR        No key yet!
                LDA     KBD             Load character. B7 should be '1'
                STA     IN,Y            Add to text buffer
                JSR     ECHO            Display character
                CMP     #CR
                BNE     NOTCR           It's not CR!

; Line received, now let's parse it

                LDY     #-1             Reset text index
                LDA     #0              Default mode is XAM
                TAX                     X=0

SETSTOR         ASL                     Leaves $7B if setting STOR mode

SETMODE         STA     MODE            Set mode flags

BLSKIP          INY                     Advance text index

NEXTITEM        LDA     IN,Y            Get character
                CMP     #CR
                BEQ     GETLINE         We're done if it's CR!
                CMP     #"."
                BCC     BLSKIP          Ignore everything below "."!
                BEQ     SETMODE         Set BLOCK XAM mode ("." = $AE)
                CMP     #":"
                BEQ     SETSTOR         Set STOR mode! $BA will become $7B
                CMP     #"I"
                BEQ     CLRRAM
                CMP     #"S"
                BEQ     START_VEC
                CMP     #"R"
                BEQ     RUN             Run the program! Forget the rest
                STX     L               Clear input value (X=0)
                STX     H
                STY     YSAV            Save Y for comparison

; Here we're trying to parse a new hex value

NEXTHEX         LDA     IN,Y            Get character for hex test
                EOR     #$B0            Map digits to 0-9
                CMP     #9+1            Is it a decimal digit?
                BCC     DIG             Yes!
                ADC     #$88            Map letter "A"-"F" to $FA-FF
                CMP     #$FA            Hex letter?
                BCC     NOTHEX          No! Character not hex

DIG             ASL
                ASL                     Hex digit to MSD of A
                ASL
                ASL

                LDX     #4              Shift count
HEXSHIFT        ASL                     Hex digit left, MSB to carry
                ROL     L               Rotate into LSD
                ROL     H               Rotate into MSD's
                DEX                     Done 4 shifts?
                BNE     HEXSHIFT        No, loop
                INY                     Advance text index
                BNE     NEXTHEX         Always taken

NOTHEX          CPY     YSAV            Was at least 1 hex digit given?
                BEQ     ESCAPE          No! Ignore all, start from scratch

                BIT     MODE            Test MODE byte
                BVC     NOTSTOR         B6=0 is STOR, 1 is XAM or BLOCK XAM

; STOR mode, save LSD of new hex byte

                LDA     L               LSD's of hex data
                STA     (STL,X)         Store current 'store index'(X=0)
                INC     STL             Increment store index.
                BNE     NEXTITEM        No carry!
                INC     STH             Add carry to 'store index' high
TONEXTITEM      JMP     NEXTITEM        Get next command item.

;--------------------------------------------------------------------------
;       Clear Memory 
;--------------------------------------------------------------------------
CLRRAM          LDA     #RTASMSG &$FF
                STA     STRL
                LDA     #RTASMSG>>8
                STA     STRH
                JSR     STRECHO                
                JSR     RAMTAS
                JSR     BINBCD16
                LDA     #NUMSTR &$FF
                STA     STRL
                LDA     #NUMSTR>>8
                STA     STRH
                JSR     STRECHO
                LDA     #MEMMSG &$FF
                STA     STRL
                LDA     #MEMMSG>>8
                STA     STRH
                JSR     STRECHO
                LDY     #$7F
                JMP     NOTCR

;-------------------------------------------------------------------------
;  RUN user's program from last opened location
;-------------------------------------------------------------------------
RUN             JMP     (XAML)          Run user's program
;-------------------------------------------------------------------------
;  We're not in Store mode
;-------------------------------------------------------------------------

NOTSTOR         BMI     XAMNEXT         B7 = 0 for XAM, 1 for BLOCK XAM

; We're in XAM mode now

                LDX     #2              Copy 2 bytes
SETADR          LDA     L-1,X           Copy hex data to
                STA     STL-1,X          'store index'
                STA     XAML-1,X         and to 'XAM index'
                DEX                     Next of 2 bytes
                BNE     SETADR          Loop unless X = 0

; Print address and data from this address, fall through next BNE.

NXTPRNT         BNE     PRDATA          NE means no address to print
                LDA     #CR             Print CR first
                JSR     ECHO
                LDA     XAMH            Output high-order byte of address
                JSR     PRBYTE
                LDA     XAML            Output low-order byte of address
                JSR     PRBYTE
                LDA     #":"            Print colon
                JSR     ECHO

PRDATA          LDA     #" "            Print space
                JSR     ECHO
                LDA     (XAML,X)        Get data from address (X=0)
                JSR     PRBYTE          Output it in hex format
XAMNEXT         STX     MODE            0 -> MODE (XAM mode).
                LDA     XAML            See if there's more to print
                CMP     L
                LDA     XAMH
                SBC     H
                BCS     TONEXTITEM      Not less! No more data to output

                INC     XAML            Increment 'examine index'
                BNE     MOD8CHK         No carry!
                INC     XAMH

MOD8CHK         LDA     XAML            If address MOD 8 = 0 start new line
                AND     #%0000.0111
                BPL     NXTPRNT         Always taken.

;-------------------------------------------------------------------------
;  Subroutine to print a byte in A in hex form (destructive)
;-------------------------------------------------------------------------

PRBYTE          PHA                     Save A for LSD
                LSR
                LSR
                LSR                     MSD to LSD position
                LSR
                JSR     PRHEX           Output hex digit
                PLA                     Restore A

; Fall through to print hex routine

;-------------------------------------------------------------------------
;  Subroutine to print a hexadecimal digit
;-------------------------------------------------------------------------

PRHEX           AND     #%0000.1111     Mask LSD for hex print
                ORA     #"0"            Add "0"
                CMP     #"9"+1          Is it a decimal digit?
                BCC     ECHO            Yes! output it
                ADC     #6              Add offset for letter A-F

; Fall through to print routine

;-------------------------------------------------------------------------
;  Subroutine to print a character to the terminal
;-------------------------------------------------------------------------

ECHO            BIT     DSP             DA bit (B7) cleared yet?
                BMI     ECHO            No! Wait for display ready
                STA     DSP             Output character. Sets DA
                RTS
;-------------------------------------------------------------------------
;  RAM TEST [Commodore 64 method]
;-------------------------------------------------------------------------
RAMTAS          TYA
                PHA
                LDA     #0
                TYA
RAMTZ0          STA     $0002,Y
                STA     $0200,Y
                STA     $0300,Y
                INY
                BNE     RAMTZ0
RAMTBT          TYA 
                LDA     #3
                STA     TMP0+1
RAMTZ1          LDA     #'.'
                JSR     ECHO
                INC     TMP0+1
RAMTZ2          LDA     (TMP0),Y                                
                TAX
                LDA     #$55
                STA     (TMP0),Y 
                CMP     (TMP0),Y 
                BNE     SIZE 
                ROL     A 
                STA     (TMP0),Y 
                CMP     (TMP0),Y 
                BNE     SIZE
                TAX
                STA     (TMP0),Y 
                INY
                BNE     RAMTZ2
                BEQ     RAMTZ1
SIZE            TYA
                TAX
                LDY     TMP0+1
                CLC
                STX     MEMSZL
                STY     MEMSZH
                LDA     #$0A
                JSR     ECHO
                LDA     #$0D
                JSR     ECHO
                PLA
                TAY
                RTS

;---------------------------------------------------------------------------------
;   String Echo
;---------------------------------------------------------------------------------

STRECHO         
                TYA
                PHA     ;-- Sanatize Y Register -------------------------------- 

                LDY     #$0
.LOOP           LDA     (STRL),Y
                BEQ     .JMPOUT
                JSR     ECHO
                INY
                BNE     .LOOP

.JMPOUT         PLA
                TAY
                RTS

;-------------------------------------------------------------------------------
;      BINBCD16 borrowed from Andrew Jacobs, 28-Feb-2004
;-------------------------------------------------------------------------------

BINBCD16
                SED                  ; Switch to decimal mode        2
                LDA     #0           ; Ensure the result is clear    2
                STA     BCD+0        ;                               3
                STA     BCD+1        ;                               3
                STA     BCD+2        ;                               3       13
 
CBIT1           ASL     MEMSZL       ; Shift out one bit             5
                ROL     MEMSZL+1     ;                               5
;               LDA     bcd+0        ;             
                ADC     BCD+0        ; And add into result           3
                STA     BCD+0        ;                               3
                ASL     MEMSZL       ;                               5
                ROL     MEMSZL+1     ;                               5
                ADC     BCD+0        ;                               3
                STA     BCD+0        ;                               3
                ASL     MEMSZL       ;                               5
                ROL     MEMSZL+1     ;                               5
                ADC     BCD+0        ;                               3
                STA     BCD+0        ;                               3
                ASL     MEMSZL       ;                               5
                ROL     MEMSZL+1     ;                               5
                ADC     BCD+0        ;                               3
                STA     BCD+0        ;                               3
                ASL     MEMSZL       ;                               5
                ROL     MEMSZL+1     ;                               5
                ADC     BCD+0        ;                               3
                STA     BCD+0        ;                               3
                ASL     MEMSZL       ;                               5
                ROL     MEMSZL+1     ;                               5
                ADC     BCD+0        ;                               3
                STA     BCD+0        ;                               3       96

                LDX     #7           ;                               2       2
CBIT7           ASL     MEMSZL       ; Shift out one bit             5
                ROL     MEMSZL+1     ;                               5
                LDA     BCD+0        ; And add into result           3
                ADC     BCD+0        ;                               3
                STA     BCD+0        ;                               3
                LDA     BCD+1        ; propagating any carry         3
                ADC     BCD+1        ;                               3
                STA     BCD+1        ;                               3
                DEX                  ; And repeat for next bit       2
                BNE     CBIT7        ;                               3       33*7-1=230

                LDX     #3           ;                               2       2
CBIT13          ASL     MEMSZL       ; Shift out one bit             5
                ROL     MEMSZL+1     ;                               5
                LDA     BCD+0        ; And add into result           3
                ADC     BCD+0        ;                               3
                STA     BCD+0        ;                               3
                LDA     BCD+1        ; propagating any carry         3
                ADC     BCD+1        ;                               3
                STA     BCD+1        ;                               3
                LDA     BCD+2        ; ... thru whole result         3
                ADC     BCD+2        ;                               3
                STA     BCD+2        ;                               3
                DEX                  ; And repeat for next bit       2
                BNE     CBIT13       ;                               3       42*3-1=125
                CLD                  ; Back to binary                2       2; tot 470
                AND     #$0F 
                ORA     #$30
                STA     NUMSTR+1
                LDA     BCD+1
                AND     #$0F
                ORA     #$30
                STA     NUMSTR+3
                LDA     BCD+1
                LSR
                LSR
                LSR
                LSR
                ORA     #$30
                STA     NUMSTR+2
                LDA     BCD+0
                AND     #$0F
                ORA     #$30
                STA     NUMSTR+5
                LDA     BCD+0
                LSR
                LSR
                LSR
                LSR
                ORA     #$30
                STA     NUMSTR+4
                LDA     #20
                STA     NUMSTR+0
                RTS                  ; All Done.
;-----------------------------------------------------------------------------------                                             
; Message Strings
;-----------------------------------------------------------------------------------
BOOTMSG         .AZ     #$01,#$01,/ VIA DEVKIT V 1.0A with WOZMON (c) Steve Wozniak 1976 (C)dannyarnold.com 2021 /,#$0D,#$02,#$0A,/[I]nitialize Memory ?/,#$0A,#$0D
MEMMSG          .AZ     / BYTES FREE/,#$0A,#$0D
RTASMSG         .AZ     /TESTING MEMORY/,#$0A,#$0D
;-------------------------------------------------------------------------
;  BOOT Vectors
;-------------------------------------------------------------------------
;               
                .NO     $FFF8
                .DA     $0000           Unused, what a pity
NMI_VEC         .DA     $0F00           NMI vector
RESET_VEC       .DA     RESET           RESET vector
IRQ_VEC         .DA     RESET           IRQ vector