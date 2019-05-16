ORG $1000

define TTY_IN  $0101
define TTY_OUT $0100

DATA {
buff:                           ; String buffer
    "xDD"
    DB $00
}

CODE {
  LDX #$FF                    ; Init stack ptr to $FF
  TXS                         ; - || -

  JSR PRINT
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
    BRK
}

IRQ{
  LDA TTY_IN
  STA TTY_OUT
}

NMI{
  BRK
}
