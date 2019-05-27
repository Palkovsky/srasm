ORG $1000

define TTY_OUT $0100
define TTY_IN $0120    
define CPU_STOP $0110

DATA {
flag:
    DB $00
buff:
    DUP 16
}

CODE {
  LDX #$FF
	TXS

WAIT:
    LDA flag
    CMP #3
    BCC *WAIT

    JMP EXIT

PRINT:
    LDY #0
PRINT_LOOP:
    LDA buff, Y
    CMP #0
    BEQ *PRINT_END
    STA TTY_OUT
    INY
    JMP PRINT_LOOP
PRINT_END:
    RTS

EXIT:
    LDA #$01
    STA CPU_STOP
    JMP EXIT
}

IRQ {
    ;;      JSR PRINT                  
    LDA TTY_IN
    STA buff
    JSR PRINT
    INC flag
    RTI
}
