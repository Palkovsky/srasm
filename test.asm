ORG $1000

define TTY_OUT $0100
define TTY_IN  $0010
define CLR $55
    
DATA {

arr1: DUP 1
    
arr2:
    DB $03
    DB $04
    DB $05
}
    
CODE {
    LDA arr1
    LDX #$02
    
    CMP arr2, X
    BCS *SUBRT_2

SUBRT_1:
    LDA #$45
    STA TTY_OUT
    JMP (EXIT)
    
SUBRT_2:
    LDA #$44
    STA $0101
    BCS *SUBRT_1
    
EXIT:
    BRK
}
