ORG $1000
    
DATA {
arr0:
arr1: DUP 1
    
arr2:
    DB $03
    DB $04
    DB $05
}
    
CODE {
    JMP (SUBRT_2)
    
    LDA #arr2
    LDX $FF
    TXS
    BRK

    JSR $FFFF
    JMP ($ABCD)
    ADC ($FF, X)
    STA arr0, Y 
    
SUBRT_1:    
    JSR $ABCD

SUBRT_2:    
    JSR $DDEE
}
