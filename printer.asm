ORG $1000

define TTY_OUT $0100
    
DATA {
sz:                             ; Size of string to print
    DB $00
buff:                           ; String buffer
    DUP $0F
}
    
CODE {

    LDX #$FF                    ; Init stack ptr to $FF
    TXS                         ; - || -

    LDA #4
    STA sz

    LDY #0
   
    LDA #$41                    ; K
    STA (sz), Y
    INY
    
    LDA #$50                    ; U
    STA (sz), Y
    INY
    
    LDA #$55                    ; P
    STA (sz), Y
    INY

    LDA #$4B                    ; A
    STA (sz), Y
    
    JSR PRINT
    JMP EXIT

PRINT:
    LDY sz
    DEY

    PRINT_LOOP: 
      CPY #00
      BMI *PRINT_END
      LDA (buff), Y
      STA TTY_OUT
      DEY
      JMP PRINT_LOOP
    
    PRINT_END:  
      RTS

EXIT:
    BRK
}
