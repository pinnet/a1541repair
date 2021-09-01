              .CR     6502
              .TF     test1541.hex,INT
              .LF     test1541.list
              .OR     $C000
              .TA     $0000
;-------------------------------------------------------------------------
; 1541 Test Rom
;-------------------------------------------------------------------------

;-------------------------------------------------------------------------
;  Memory declaration
;-------------------------------------------------------------------------

;-------------------------------------------------------------------------
;  Constants
;-------------------------------------------------------------------------


;-------------------------------------------------------------------------
;  Let's get started
;
;  Remark the RESET routine is only to be entered by asserting the RESET
;  line of the system. This ensures that the data direction registers
;  are selected.
;-------------------------------------------------------------------------
RESET       CLD
            LDA #$08
            ORA $1C00
            STA $1C00     turn LED on
            STA $1C02
            
blink       LDA #$00
            TAY

delay       CLC 
loop1       ADC #$01 
            BNE loop1
            DEY
            BNE delay

            LDA $1C00
            AND #$F7      turn LED off
            STA $1C00
            
            
            LDA #$00
            TAY
delay2      CLC 
loop2       ADC #$01 
            BNE loop1
            DEY
            BNE delay2
            
            LDA #$08
            ORA $1C00
            STA $1C00

            jmp blink





            jmp RESET            
IRQ
NMI
HALTLOOP        jmp     HALTLOOP


;-------------------------------------------------------------------------
;  Vector area
;-------------------------------------------------------------------------
                .NO     $FFF8
                .DA     $0000           Unused, what a pity
NMI_VEC         .DA     NMI             NMI vector
RESET_VEC       .DA     RESET           RESET vector
IRQ_VEC         .DA     IRQ             IRQ vector